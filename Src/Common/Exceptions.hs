module Src.Common.Exceptions where

import Control.Monad.Except
import Src.Shiro.Abs
import Src.TypeChecker.Operators
import Src.Common.Types

type IOWithExceptions = ExceptT ExceptionHolder IO

data ExceptionHolder
    = ParserException String
    | TypeCheckerException BNFC'Position TypeCheckerExceptionType
    | RuntimeException BNFC'Position RuntimeExceptionType
    | ControlledExit Int

instance Show ExceptionHolder where
    show (ParserException exception) = "Parser exception: " ++ show exception
    show (TypeCheckerException pos exceptionT) = concat ["TypeChecker exception at ", showPos pos, ": ", show exceptionT]
    show (RuntimeException pos exceptionT) = concat ["Runtime exception at ", showPos pos, ": ", show exceptionT]
    show (ControlledExit statusCode)
        | statusCode /= 0 = concat ["Program exited with code ", show statusCode, "."]
        | otherwise = ""

showPos :: BNFC'Position -> String
showPos (BNFC'Position line column) = concat ["[Ln ", show line, ", Col ", show column, "]"]
showPos BNFC'NoPosition = "[unknown position]"
showPos pos = show pos

data TypeCheckerExceptionType
    = ExptMathDivisionByZero
    | ExptMathModuloZero
    | ExptTopLevelProgramReturns
    | ExptValNotDeclared Ident
    | ExptValAlreadyDeclared Ident
    | ExptFunCallNotCallableType ValType
    | ExptFunNotDeclared Ident
    | ExptFunParamsWrongCount Int Int
    | ExptFunParamWrongType ValType ValType
    | ExptFunParamExprRefPass
    | ExptFunParamConstMutablePass
    | ExptFunReturnNoReturn Ident ValType
    | ExptFunReturnUnitReturns Ident ValType
    | ExptFunReturnNotAllBranchesReturn Ident ValType
    | ExptFunReturnDifferentTypes ValType ValType
    | ExptFunReturnWrongType Ident ValType ValType
    | ExptModifyConst Ident ValType
    | ExptModifyFun Ident ValType
    | ExptModifyWrongType Ident ValType ValType
    | ExptWrongTypeOpParam Ident ValType ValType AssAddOneOp
    | ExptWrongTypeUnOpParam UnOperator ValType
    | ExptWrongTypeBiOpParams BiOperator ValType ValType
    | ExptWrongTypeCondExpr ValType
    | ExptWrongTypeForRangeExpr ValType
    | ExptTernaryExprsOfDifferentType ValType ValType
    deriving (Eq)

instance Show TypeCheckerExceptionType where
    show ExptMathDivisionByZero = "Division by zero is not allowed."
    show ExptMathModuloZero = "Modulo zero is not defined."
    show ExptTopLevelProgramReturns = "Top-level program cannot return value.\n(Possibly wanted to use exit: (Int) -> Unit)"
    show (ExptValNotDeclared ident) = concat ["Value ", showIdent ident, " is not declared in the scope."]
    show (ExptValAlreadyDeclared i) = concat ["Value ", showIdent i, " was already declared in its scope."]
    show (ExptFunCallNotCallableType t) = concat ["Tried to use function application on the non-callable expression of type ", show t, "."]
    show (ExptFunNotDeclared ident) = concat ["Function ", showIdent ident, " is not declared in the scope."]
    show (ExptFunParamsWrongCount ex n) = concat ["Callable expression called with ", show n, " parameters but expected ", show ex, "."]
    show (ExptFunParamWrongType t ex) = concat ["Function parameter is of type ", show t, " but expected ", show ex, "."]
    show ExptFunParamExprRefPass = "Expression was passed to a mutable reference parameter."
    show ExptFunParamConstMutablePass = "Immutable value was passed to a mutable reference parameter."
    show (ExptFunReturnNoReturn i t) = concat ["Function ", show i, " does not have a return statement but is expected to return a value of type ", show t, "."]
    show (ExptFunReturnUnitReturns i t) = concat ["Function ", show i, " sometimes returns a value of type ", show t, " but should not return or return Unit."]
    show (ExptFunReturnNotAllBranchesReturn i t) = concat ["Not all branches of function ", show i, " return a value of type ", show t, "."]
    show (ExptFunReturnDifferentTypes t1 t2) = concat ["Tried to return value of type ", show t1, " but in the same return scope tried to return another value of type", show t2, "."]
    show (ExptFunReturnWrongType i t1 t2)
        | t2 == ValTypeUnit && i == Ident "λ" =
            concat
                [ "Function λ"
                , " returns a value of type "
                , show t1
                , " but is expected to return a value of type "
                , show t2
                , ".\n"
                , "(Possibly forgot to explicitly provide type annotation after lambda expression parameters)"
                ]
        | otherwise = concat [show i, " returns a value of type ", show t1, " but is expected to return a value of type ", show t2, "."]
    show (ExptModifyConst name valType) = concat ["Cannot modify const: ", showIdentAndValType name valType, "."]
    show (ExptModifyFun name valType) = concat ["Cannot reassign non-lambda function ", showIdentAndValType name valType, "."]
    show (ExptModifyWrongType ident typeVal exprType) =
        concat ["Cannot assign expression of type ", show exprType, " to ", showIdentAndValType ident typeVal, "."]
    show (ExptWrongTypeOpParam i t ex op) = concat ["Operator ", show op, " should be applied to type ", show ex, " but ", showIdent i, " is of type ", show t, "."]
    show (ExptWrongTypeUnOpParam op t) = concat ["Unary operator ", show op, " is not defined for type ", show t, "."]
    show (ExptWrongTypeBiOpParams op t1 t2) = concat ["Binary operator ", show op, " is not defined for types ", show t1, " and ", show t2, "."]
    show (ExptWrongTypeCondExpr t) = concat ["Condition should be of type boolean but got ", show t, "."]
    show (ExptWrongTypeForRangeExpr t) = concat ["For range bound expression should be of type Int but got ", show t, "."]
    show (ExptTernaryExprsOfDifferentType t1 t2) = concat ["Ternary operator expressions should be of the same type but got ", show t1, " and ", show t2, "."]

data RuntimeExceptionType
    = ExptMathDivisionByZeroRuntime
    | UnexpectedExceptionRuntime
    deriving (Eq)

instance Show RuntimeExceptionType where
    show ExptMathDivisionByZeroRuntime = "Division by zero is not allowed."
    show UnexpectedExceptionRuntime = "Got unexpected runtime exception."

showIdentAndValType :: Ident -> ValType -> String
showIdentAndValType (Ident ident) valType = concat ["'", ident, ": ", show valType, "'"]

showIdent :: Ident -> String
showIdent (Ident s) = concat ["'\"'", s, "'\"'"]
