{-# LANGUAGE DataKinds #-}

module Src.Evaluator.Evaluator where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Src.Common.Environment
import Src.Common.Exceptions
import Src.Common.Types
import Src.Evaluator.Environment
import Src.Evaluator.StdLib
import Src.Shiro.Abs
import Src.Evaluator.Types
import Src.Evaluator.Store

evalProgram :: Program -> IO (Either ExceptionHolder ())
evalProgram (PProgram _ instructions) = do
    let (env, store) = makeStdLib envEmpty storeEmpty
    evalStateT (runReaderT (runExceptT $ void $ evalInstr instructions) env) store

evalInstr :: [Instr] -> EvaluatorM RetType
evalInstr [] = pure Nothing
evalInstr ((InstExpr _ expr) : instructions) = evalExpr expr >> evalInstr instructions
evalInstr ((InstDecl _ declr) : instructions) = do
    modEnv <- evalDeclr declr
    local modEnv $ evalInstr instructions
evalInstr ((InstAssAddOne _ ident (OpIncr _)) : instructions) = evalIntOp ident (+ 1) >> evalInstr instructions
evalInstr ((InstAssAddOne _ ident (OpDecr _)) : instructions) = evalIntOp ident (subtract 1) >> evalInstr instructions
evalInstr ((InstAss _ ident expr) : instructions) = do
    value <- evalExpr expr
    loc <- envGet ident =<< ask
    modify $ storeInsert [(loc, value)]
    evalInstr instructions
evalInstr ((InstRetExpr _ expr) : _) = do
    retValue <- evalExpr expr
    pure $ Just retValue
evalInstr ((InstRetUnit _) : _) = pure $ Just ValUnit
evalInstr ((InstCond _ (CondIfElse _ expr block1 block2)) : instructions) = evalCondInstr expr block1 block2 instructions
evalInstr ((InstCond _ (CondIf pos expr block)) : instructions) = evalCondInstr expr block (BlockInstr pos []) instructions
evalInstr ((InstCond _ (CondIfElseIf pos expr block1 elseIf)) : instructions) = evalCondInstr expr block1 (BlockInstr pos [InstCond pos elseIf]) instructions
evalInstr whileLoop@((InstWhile _ expr block) : instructions) = do
    (ValBool cond) <- evalExpr expr
    if cond
        then do
            ret <- evalBlock block
            if isJust ret
                then pure ret
                else evalInstr whileLoop
        else evalInstr instructions
evalInstr ((InstFor _ ident startExpr endExpr block) : instructions) = do
    (ValInt startVal) <- evalExpr startExpr
    (ValInt endVal) <- evalExpr endExpr
    let range = [startVal .. endVal]
    [loc] <- getNewLocs 1
    local (envInsert [(ident, loc)]) (evalFor loc range block) >>= evalRetType instructions
evalInstr ((InstBlock _ block) : instructions) = evalBlock block >>= evalRetType instructions

evalCondInstr :: Expr -> Block -> Block -> [Instr] -> EvaluatorM RetType
evalCondInstr expr block1 block2 instructions = do
    retTypeB <- evalExpr expr
    case retTypeB of
        ValBool condition ->
            if condition
                then evalBlock block1 >>= evalRetType instructions
                else evalBlock block2 >>= evalRetType instructions
        _ -> throwError $ RuntimeException Nothing UnexpectedExceptionRuntime

evalFor :: Loc -> [Int] -> Block -> EvaluatorM RetType
evalFor _ [] _ = pure Nothing
evalFor loc (index : indexes) block = do
    modify $ storeInsert [(loc, ValInt index)]
    ret <- evalBlock block
    if isJust ret
        then pure ret
        else evalFor loc indexes block

evalRetType :: [Instr] -> RetType -> EvaluatorM RetType
evalRetType instructions Nothing = evalInstr instructions
evalRetType _ ret = pure ret

evalIntOp :: Ident -> (Int -> Int) -> EvaluatorM ()
evalIntOp ident op = do
    loc <- envGet ident =<< ask
    store <- get
    (ValInt val) <- storeGet loc store
    modify $ storeInsert [(loc, ValInt $ op val)]

evalDeclr :: Declr -> EvaluatorM ModEvaluatorEnv
evalDeclr (DeclVal _ vars) = do
    items <- mapM evalDeclrExpr vars
    addItemsToEnv items
evalDeclr (DeclConst _ vars) = do
    items <- mapM evalDeclrExpr vars
    addItemsToEnv items
evalDeclr (DeclFunVal pos ident params _ block) = evalDeclr (DeclFunUnitAuto pos ident params block)
evalDeclr (DeclFunUnit pos ident params _ block) = evalDeclr (DeclFunUnitAuto pos ident params block)
evalDeclr (DeclFunUnitAuto _ ident params block) = do
    let params' = map resolveDeclParam params
    env <- ask
    [loc] <- getNewLocs 1
    let valFun = ValFun params' (UserDefined block) (envInsert [(ident, loc)] env)
    modify $ storeInsert [(loc, valFun)]
    pure $ envInsert [(ident, loc)]

evalDeclrExpr :: DeclrItem -> EvaluatorM (Ident, Value)
evalDeclrExpr (DeclItemType _ ident _ expr) = do
    value <- evalExpr expr
    pure (ident, value)

resolveDeclParam :: Param -> (Ident, ValPassingMechanism)
resolveDeclParam (ParamDecl _ ident (ParamTypeMutableCopy _ _)) = (ident, ValCopy)
resolveDeclParam (ParamDecl _ ident (ParamTypeConstCopy _ _)) = (ident, ValCopy)
resolveDeclParam (ParamDecl _ ident (ParamTypeConstRef _ _)) = (ident, ValReference)
resolveDeclParam (ParamDecl _ ident (ParamTypeRefConst _ _)) = (ident, ValReference)
resolveDeclParam (ParamDecl _ ident (ParamTypeMutableRef _ _)) = (ident, ValReference)

evalBlock :: Block -> EvaluatorM RetType
evalBlock (BlockInstr _ instructions) = evalInstr instructions

evalBiIntOpRNonZero :: (Int -> Int -> Int) -> BNFC'Position -> Int -> Int -> EvaluatorM Value
evalBiIntOpRNonZero op pos n1 n2 =
    if n2 == 0
        then throwError $ RuntimeException pos ExptMathDivisionByZeroRuntime
        else pure $ ValInt $ op n1 n2

evalExpr :: Expr -> EvaluatorM Value
evalExpr (ExprValName _ ident) = getIdentVal ident
evalExpr (ExprLitInt _ n) = pure $ ValInt $ fromInteger n
evalExpr (ExprLitTrue _) = pure $ ValBool True
evalExpr (ExprLitFalse _) = pure $ ValBool False
evalExpr (ExprLitString _ s) = pure $ ValString s
evalExpr (ExprNeg _ (OpNeg _) expr) = do
    (ValInt n) <- evalExpr expr
    pure (ValInt $ negate n)
evalExpr (ExprNot _ (OpNot _) expr) = do
    (ValBool block) <- evalExpr expr
    pure (ValBool $ not block)
evalExpr (ExprMul _ e1 op e2) = do
    (ValInt n1) <- evalExpr e1
    (ValInt n2) <- evalExpr e2
    case op of
        OpTimes _ -> pure (ValInt $ n1 * n2)
        OpDiv pos -> evalBiIntOpRNonZero div pos n1 n2
        OpMod pos -> evalBiIntOpRNonZero mod pos n1 n2
evalExpr (ExprAdd pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (op, v1, v2) of
        (OpPlus _, ValInt n1, ValInt n2) -> pure (ValInt $ n1 + n2)
        (OpMinus _, ValInt n1, ValInt n2) -> pure (ValInt $ n1 - n2)
        (OpPlus _, ValString s1, ValString s2) -> pure (ValString $ s1 ++ s2)
        _ -> throwError $ RuntimeException pos UnexpectedExceptionRuntime
evalExpr (ExprRel _ e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    block <- evalRelOp op v1 v2
    pure $ ValBool block
evalExpr (ExprAnd _ e1 (OpAnd _) e2) = do
    (ValBool bool1) <- evalExpr e1
    if bool1
        then do
            (ValBool bool2) <- evalExpr e2
            pure $ ValBool bool2
        else pure $ ValBool False
evalExpr (ExprOr _ e1 (OpOr _) e2) = do
    (ValBool bool1) <- evalExpr e1
    if bool1
        then pure $ ValBool True
        else do
            (ValBool bool2) <- evalExpr e2
            pure $ ValBool bool2
evalExpr (ExprTernary _ conditionExpr e1 e2) = do
    (ValBool condition) <- evalExpr conditionExpr
    if condition
        then evalExpr e1
        else evalExpr e2
evalExpr (ExprApp pos ident callParams) = do
    (ValFun params block env) <- evalExpr ident
    argsVMl <- mapM evalRef callParams
    let (argsV, argsMl) = unzip argsVMl
    let (argsI, argsR) = unzip params
    argsL <- mapM insertArg $ zip3 argsR argsV argsMl
    let envInserter = envInsert $ zip argsI argsL
    ret <- local (\_ -> envInserter env) $ evalFunBlock pos block
    case ret of
        Nothing -> pure ValUnit
        Just value -> pure value
  where
    insertArg :: (ValPassingMechanism, Value, Maybe Loc) -> EvaluatorM Loc
    insertArg (valPassingMechanism, value, maybeLoc) = do
        loc <- case valPassingMechanism of
            ValReference -> do
                let Just loc = maybeLoc
                pure loc
            ValCopy -> do
                [loc] <- getNewLocs 1
                pure loc
        modify $ storeInsert [(loc, value)]
        pure loc
evalExpr (ExprLambdaVal _ params _ block) = do
    let params' = map resolveDeclParam params
    asks $ ValFun params' (UserDefined block)
evalExpr (ExprLambdaUnit _ params _ block) = do
    let params' = map resolveDeclParam params
    asks $ ValFun params' (UserDefined block)
evalExpr (ExprLambdaUnitAuto _ params block) = do
    let params' = map resolveDeclParam params
    asks $ ValFun params' (UserDefined block)

evalRef :: Expr -> EvaluatorM (Value, Maybe Loc)
evalRef (ExprValName _ ident) = envGetValueAndLocFor ident
evalRef expr = do
    value <- evalExpr expr
    pure (value, Nothing)

evalRelOp :: RelOp -> Value -> Value -> EvaluatorM Bool
evalRelOp (OpEq _) (ValInt n1) (ValInt n2) = pure $ n1 == n2
evalRelOp (OpNe _) (ValInt n1) (ValInt n2) = pure $ n1 /= n2
evalRelOp (OpLt _) (ValInt n1) (ValInt n2) = pure $ n1 < n2
evalRelOp (OpGt _) (ValInt n1) (ValInt n2) = pure $ n1 > n2
evalRelOp (OpLe _) (ValInt n1) (ValInt n2) = pure $ n1 <= n2
evalRelOp (OpGe _) (ValInt n1) (ValInt n2) = pure $ n1 >= n2
evalRelOp (OpEq _) (ValBool block1) (ValBool block2) = pure $ block1 == block2
evalRelOp (OpNe _) (ValBool block1) (ValBool block2) = pure $ block1 /= block2
evalRelOp (OpEq _) (ValString s1) (ValString s2) = pure $ s1 == s2
evalRelOp (OpNe _) (ValString s1) (ValString s2) = pure $ s1 /= s2
evalRelOp _ _ _ = throwError $ RuntimeException Nothing UnexpectedExceptionRuntime

evalFunBlock :: BNFC'Position -> FunBlock -> EvaluatorM RetType
evalFunBlock _ (UserDefined block) = evalBlock block
evalFunBlock pos PrintString = do
    (ValString s) <- evalExpr (ExprValName pos (Ident "s"))
    liftIO $ putStr s
    pure Nothing
evalFunBlock pos PrintInt = do
    (ValInt n) <- evalExpr (ExprValName pos (Ident "i"))
    liftIO $ putStr $ show n
    pure Nothing
evalFunBlock pos PrintBool = do
    (ValBool n) <- evalExpr (ExprValName pos (Ident "b"))
    liftIO $ putStr $ show n
    pure Nothing
evalFunBlock pos Exit = do
    (ValInt n) <- evalExpr (ExprValName pos (Ident "n"))
    throwError $ ControlledExit n
