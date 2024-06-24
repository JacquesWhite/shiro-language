module Src.TypeChecker.TypeChecker where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Src.Common.Exceptions
import Src.Shiro.Abs
import Src.TypeChecker.Types

import Data.Functor ((<&>))
import Src.Common.Environment
import Src.Common.Types
import Src.Shiro.Par
import Src.TypeChecker.Environment
import Src.TypeChecker.Operators
import Src.TypeChecker.Params
import Src.TypeChecker.StdLib

data ReturnType
    = None
    | Definitive ValType
    | Branching ValType

type ParserM a = Either ExceptionHolder a

parseProgram :: String -> ParserM Program
parseProgram code = do
    let tokens = myLexer code
        parserResult = pProgram tokens
    case parserResult of
        Left errorStr -> Left (ParserException errorStr)
        Right program -> Right program

typeCheck :: Program -> ParserM ()
typeCheck program = evalState (runExceptT (checkTypeProg program)) stdLib

checkTypeProg :: Program -> TypeCheckerM ()
checkTypeProg (PProgram pos instructions) = do
    retType <- checkTypeBlock (BlockInstr pos instructions)
    case retType of
        None -> pure ()
        _ -> throwError $ TypeCheckerException pos ExptTopLevelProgramReturns

checkTypeInstr :: Instr -> TypeCheckerM ReturnType
checkTypeInstr (InstExpr _ expr) = checkTypeExpr expr >> pure None
checkTypeInstr (InstDecl _ declr) = checkTypeDeclr declr
checkTypeInstr (InstBlock _ block) = checkTypeBlock block
checkTypeInstr (InstAssAddOne pos ident op) = opOnValType pos ident ValTypeInt op
checkTypeInstr (InstAss pos ident expr) = do
    (valType, mutability) <- getValType pos ident
    exprType <- checkTypeExpr expr
    if mutability == ValConst
        then case valType of
            ValTypeFun _ _ -> throwError $ TypeCheckerException pos $ ExptModifyFun ident valType
            _ -> throwError $ TypeCheckerException pos $ ExptModifyConst ident valType
        else
            if valType /= exprType
                then throwError $ TypeCheckerException pos $ ExptModifyWrongType ident valType exprType
                else pure None
checkTypeInstr (InstRetExpr _ expr) = checkTypeExpr expr <&> Definitive
checkTypeInstr (InstRetUnit _) = pure $ Definitive ValTypeUnit
checkTypeInstr (InstCond _ condInstr) = checkTypeCond condInstr
checkTypeInstr (InstWhile pos expr block) = checkTypeCond (CondIfElse pos expr block (BlockInstr pos []))
checkTypeInstr (InstFor pos ident startExpr endExpr forBody) = do
    startExprType <- checkTypeExpr startExpr
    endExprType <- checkTypeExpr endExpr
    case (startExprType, endExprType) of
        (ValTypeInt, ValTypeInt) ->
            localState (envInsert [(ident, (ValTypeInt, ValConst))]) $
                checkTypeInstr (InstWhile pos (ExprLitTrue pos) forBody)
        (ValTypeInt, _) -> throwError $ TypeCheckerException pos $ ExptWrongTypeForRangeExpr endExprType
        (_, _) -> throwError $ TypeCheckerException pos $ ExptWrongTypeForRangeExpr startExprType

checkTypeCond :: CondInstr -> TypeCheckerM ReturnType
checkTypeCond (CondIfElse pos condExpr block1 block2) = checkTypeCondHelper pos condExpr block1 block2
checkTypeCond (CondIf pos expr block) = checkTypeCondHelper pos expr block (BlockInstr pos [])
checkTypeCond (CondIfElseIf pos condExpr block nextIf) = checkTypeCondHelper pos condExpr block (BlockInstr pos [InstCond pos nextIf])

checkTypeCondHelper :: BNFC'Position -> Expr -> Block -> Block -> TypeCheckerM ReturnType
checkTypeCondHelper pos conditionExpr ifBlock1 ifBlock2 = do
    conditionExpr' <- checkTypeExpr conditionExpr
    if conditionExpr' /= ValTypeBool
        then throwError $ TypeCheckerException pos $ ExptWrongTypeCondExpr conditionExpr'
        else do
            block1Ret <- checkTypeBlock ifBlock1
            block2Ret <- checkTypeBlock ifBlock2
            case (block1Ret, block2Ret) of
                (None, None) -> pure None
                (Definitive valType, None) -> pure $ Branching valType
                (None, Definitive valType) -> pure $ Branching valType
                (Branching valType, None) -> pure $ Branching valType
                (None, Branching valType) -> pure $ Branching valType
                (Definitive valType1, Definitive valType2) -> go valType1 valType2 Definitive
                (Branching valType1, Branching valType2) -> go valType1 valType2 Branching
                (Definitive valType1, Branching valType2) -> go valType1 valType2 Branching
                (Branching valType1, Definitive valType2) -> go valType1 valType2 Branching
  where
    go :: ValType -> ValType -> (ValType -> ReturnType) -> TypeCheckerM ReturnType
    go valType1 valType2 f =
        if valType1 == valType2
            then pure $ f valType1
            else throwError $ TypeCheckerException pos $ ExptFunReturnDifferentTypes valType1 valType2

checkTypeBlock :: Block -> TypeCheckerM ReturnType
checkTypeBlock (BlockInstr _ instructions) = localState id $ checkTypeBlockHelper None instructions

checkTypeBlockHelper :: ReturnType -> [Instr] -> TypeCheckerM ReturnType
checkTypeBlockHelper prev [] = pure prev
checkTypeBlockHelper prev (instruction : instructions) = do
    instrRetType <- checkTypeInstr instruction
    let pos = hasPosition instruction
    merged <- mergeReturnTypes pos prev instrRetType
    checkTypeBlockHelper merged instructions

checkTypeDeclr :: Declr -> TypeCheckerM ReturnType
checkTypeDeclr (DeclVal pos declrItems) = declareNewVariables pos declrItems ValMutable
checkTypeDeclr (DeclConst pos declrItems) = declareNewVariables pos declrItems ValConst
checkTypeDeclr (DeclFunUnitAuto pos fName params block) = checkTypeDeclr (DeclFunUnit pos fName params (UnitType pos) block)
checkTypeDeclr (DeclFunUnit pos fName params _ block) = do
    params' <- mapM resolveFunParam params
    checkParamNames Set.empty $ map (\(ident, valType', _, _, p) -> (ident, valType', p)) params'
    let funRet = ValTypeUnit
    checkTypeFunHelper pos fName params' funRet block
checkTypeDeclr (DeclFunVal pos fName params valType block) = do
    params' <- mapM resolveFunParam params
    checkParamNames Set.empty $ map (\(ident, valType', _, _, p) -> (ident, valType', p)) params'
    let funRet = absTypeToValType valType
    checkTypeFunHelper pos fName params' funRet block

checkTypeFunHelper :: BNFC'Position -> Ident -> [FunParamWithIdentAndPos] -> ValType -> Block -> TypeCheckerM ReturnType
checkTypeFunHelper pos ident params retType block = do
    saveEnv <- get
    let f = ValTypeFun (map funParamWithoutIdentAndPos params) retType
    modify $ modifyEnvForFunction [(ident, (f, ValConst))] params
    actualRetType <- checkTypeBlock block
    case actualRetType of
        None -> when (retType /= ValTypeUnit) $ throwError $ TypeCheckerException pos $ ExptFunReturnNoReturn ident retType
        Definitive valType' -> when (valType' /= retType) $ throwError $ TypeCheckerException pos $ ExptFunReturnWrongType ident valType' retType
        Branching valType' ->
            if valType' /= retType
                then throwError $ TypeCheckerException pos $ ExptFunReturnWrongType ident valType' retType
                else when (retType /= ValTypeUnit) $ throwError $ TypeCheckerException pos $ ExptFunReturnNotAllBranchesReturn ident retType
    put saveEnv
    modify $ envUnion $ Map.fromList [(ident, (f, ValConst))]
    pure None

checkTypeDeclrItem :: DeclrItem -> TypeCheckerM (Ident, ValType, BNFC'Position)
checkTypeDeclrItem (DeclItemType pos ident valType expr) = do
    let valType' = absTypeToValType valType
    exprType' <- checkTypeExpr expr
    if valType' == exprType'
        then pure (ident, valType', pos)
        else throwError $ TypeCheckerException pos $ ExptModifyWrongType ident valType' exprType'

checkTypeExpr :: Expr -> TypeCheckerM ValType
checkTypeExpr (ExprValName pos ident) = fst <$> getValType pos ident
checkTypeExpr (ExprLitInt _ _) = pure ValTypeInt
checkTypeExpr (ExprLitTrue _) = pure ValTypeBool
checkTypeExpr (ExprLitFalse _) = pure ValTypeBool
checkTypeExpr (ExprLitString _ _) = pure ValTypeString
checkTypeExpr (ExprNeg pos op expr) = checkExprSingleOp (opNegToBiOperator op) pos expr
checkTypeExpr (ExprNot pos op expr) = checkExprSingleOp (opNotToBiOperator op) pos expr
checkTypeExpr (ExprMul pos _ (OpDiv _) (ExprLitInt _ 0)) = throwError $ TypeCheckerException pos ExptMathDivisionByZero
checkTypeExpr (ExprMul pos e1 op e2) = checkExprBiOp (mulOpToBiOperator op) pos e1 e2
checkTypeExpr (ExprAdd pos e1 op e2) = checkExprBiOp (addOpToBiOperator op) pos e1 e2
checkTypeExpr (ExprRel pos e1 op e2) = checkExprBiOp (relOpToBiOperator op) pos e1 e2
checkTypeExpr (ExprOr pos e1 op e2) = checkExprBiOp (orOpToBiOperator op) pos e1 e2
checkTypeExpr (ExprAnd pos e1 op e2) = checkExprBiOp (andOpToBiOperator op) pos e1 e2
checkTypeExpr (ExprApp pos callableExpr paramExprs) = do
    fType <- checkTypeExpr callableExpr
    (params, ret) <- deconstructFunType pos fType
    callParams <- mapM (getCallExprType pos) paramExprs
    if length params /= length callParams
        then throwError $ TypeCheckerException pos $ ExptFunParamsWrongCount (length params) (length callParams)
        else do
            let params' = zip callParams params
            mapM_ (checkCallParam pos) params'
            pure ret
checkTypeExpr (ExprTernary pos conditionExpr expr1 expr2) = do
    expr1Type <- checkTypeExpr expr1
    expr2Type <- checkTypeExpr expr2
    ternaryCondExprType <- checkTypeExpr conditionExpr
    if ternaryCondExprType /= ValTypeBool
        then throwError $ TypeCheckerException pos $ ExptWrongTypeCondExpr ternaryCondExprType
        else
            if expr1Type /= expr2Type
                then throwError $ TypeCheckerException pos $ ExptTernaryExprsOfDifferentType expr1Type expr2Type
                else pure expr1Type
checkTypeExpr (ExprLambdaVal pos params lambdaRetType lambdaBody) = do
    let fName = Ident " λ" -- lambda symbol
    params' <- mapM resolveFunParam params
    checkParamNames Set.empty $ map (\(ident, type', _, _, pos') -> (ident, type', pos')) params'
    let funParams = map (\(_, type', mutability, passingMechanism, _) -> (type', mutability, passingMechanism)) params'
    let funRet = absTypeToValType lambdaRetType
    saveEnv <- get
    modify $ modifyEnvForFunction [] params'
    retType <- checkTypeBlock lambdaBody
    _ <- case retType of
        None -> throwError $ TypeCheckerException pos $ ExptFunReturnNoReturn fName funRet
        Definitive type' -> when (type' /= funRet) $ throwError $ TypeCheckerException pos $ ExptFunReturnWrongType fName type' funRet
        Branching type' -> throwError $ TypeCheckerException pos $ ExptFunReturnWrongType fName type' funRet
    put saveEnv
    pure $ ValTypeFun funParams funRet
checkTypeExpr (ExprLambdaUnit pos params _ lambdaBody) = checkTypeExpr (ExprLambdaUnitAuto pos params lambdaBody)
checkTypeExpr (ExprLambdaUnitAuto pos params lambdaBody) = do
    let fName = Ident "λ" -- lambda symbol
    params' <- mapM resolveFunParam params
    checkParamNames Set.empty $ map (\(ident, type', _, _, pos') -> (ident, type', pos')) params'
    let funParams = map (\(_, type', mutability, passingMechanism, _) -> (type', mutability, passingMechanism)) params'
    saveEnv <- get
    modify $ modifyEnvForFunction [] params'
    retType <- checkTypeBlock lambdaBody
    _ <- case retType of
        None -> pure ValTypeUnit
        Definitive type' ->
            if type' /= ValTypeUnit
                then throwError $ TypeCheckerException pos $ ExptFunReturnWrongType fName type' ValTypeUnit
                else pure ValTypeUnit
        Branching type' ->
            if type' /= ValTypeUnit
                then throwError $ TypeCheckerException pos $ ExptFunReturnUnitReturns fName type'
                else pure ValTypeUnit
    put saveEnv
    pure $ ValTypeFun funParams ValTypeUnit

getValType :: BNFC'Position -> Ident -> TypeCheckerM ValProps
getValType pos ident = do
    env <- get
    case Map.lookup ident env of
        Just (valType, mutability) -> pure (valType, mutability)
        Nothing -> throwError $ TypeCheckerException pos $ ExptValNotDeclared ident

deconstructFunType :: BNFC'Position -> ValType -> TypeCheckerM ([FunParamType], ValType)
deconstructFunType _ (ValTypeFun params ret) = pure (params, ret)
deconstructFunType pos valType = throwError $ TypeCheckerException pos $ ExptFunCallNotCallableType valType

getCallExprType :: BNFC'Position -> Expr -> TypeCheckerM (ValType, Maybe ValMutability)
getCallExprType _ (ExprValName _ ident) = do
    (valType, mutability) <- getValType undefined ident
    pure (valType, Just mutability)
getCallExprType _ expr = do
    valType <- checkTypeExpr expr
    pure (valType, Nothing)

opOnValType :: BNFC'Position -> Ident -> ValType -> AssAddOneOp -> TypeCheckerM ReturnType
opOnValType pos ident expectedType op = do
    (valType, mutability) <- getValType pos ident
    case valType of
        t | t == expectedType -> case mutability of
            ValMutable -> pure None
            ValConst -> throwError $ TypeCheckerException pos $ ExptModifyConst ident expectedType
        _ -> throwError $ TypeCheckerException pos $ ExptWrongTypeOpParam ident valType expectedType op

checkParamNames :: Set.Set Ident -> [(Ident, ValType, BNFC'Position)] -> TypeCheckerM ()
checkParamNames _ [] = pure ()
checkParamNames indentsSet ((ident, _, pos) : rest) = do
    if Set.member ident indentsSet
        then throwError $ TypeCheckerException pos $ ExptValAlreadyDeclared ident
        else checkParamNames (Set.insert ident indentsSet) rest

declareNewVariables :: BNFC'Position -> [DeclrItem' BNFC'Position] -> ValMutability -> TypeCheckerM ReturnType
declareNewVariables _ declrItems mutability = do
    items <- mapM checkTypeDeclrItem declrItems
    checkParamNames Set.empty items
    mapM_ (\(ident, valType, _) -> modify (envInsert [(ident, (valType, mutability))])) items
    pure None

checkExprSingleOp :: UnOperator -> BNFC'Position -> Expr' BNFC'Position -> TypeCheckerM ValType
checkExprSingleOp op pos expr = do
    expr1Type <- checkTypeExpr expr
    case unOpTypeFor op expr1Type of
        Just valType -> pure valType
        Nothing -> throwError $ TypeCheckerException pos $ ExptWrongTypeUnOpParam op expr1Type

checkExprBiOp :: BiOperator -> BNFC'Position -> Expr' BNFC'Position -> Expr' BNFC'Position -> TypeCheckerM ValType
checkExprBiOp op pos expr1 expr2 = do
    expr1Type <- checkTypeExpr expr1
    expr2Type <- checkTypeExpr expr2
    case biOpTypeFor op (expr1Type, expr2Type) of
        Just valType -> pure valType
        Nothing -> throwError $ TypeCheckerException pos $ ExptWrongTypeBiOpParams op expr1Type expr2Type

mergeReturnTypes :: BNFC'Position -> ReturnType -> ReturnType -> TypeCheckerM ReturnType
mergeReturnTypes pos retType1 retType2 = do
    case (retType1, retType2) of
        (None, _) -> pure retType2
        (_, None) -> pure retType1
        (Branching retType1', Definitive retType2') -> merge retType1' retType2' Definitive
        (Branching retType1', Branching retType2') -> merge retType1' retType2' Branching
        (Definitive retType1', Branching retType2') -> merge retType1' retType2' Definitive
        (Definitive retType1', Definitive retType2') -> merge retType1' retType2' Definitive
        >>= \retType -> pure retType
  where
    merge :: ValType -> ValType -> (ValType -> ReturnType) -> TypeCheckerM ReturnType
    merge retType1' retType2' f =
        if retType1' == retType2'
            then pure $ f retType1'
            else throwError $ TypeCheckerException pos $ ExptFunReturnDifferentTypes retType1' retType2'
