module Src.TypeChecker.Params where

import Control.Monad.Except (MonadError (throwError))
import Src.Common.Exceptions
import Src.Common.Types
import Src.Shiro.Abs
import Src.TypeChecker.Environment
import Src.TypeChecker.Types

resolveFunParam :: Param -> TypeCheckerM FunParamWithIdentAndPos
resolveFunParam (ParamDecl pos ident (ParamTypeMutableCopy _ valType)) = pure (ident, absTypeToValType valType, ValMutable, ValCopy, pos)
resolveFunParam (ParamDecl pos ident (ParamTypeMutableRef _ valType)) = pure (ident, absTypeToValType valType, ValMutable, ValReference, pos)
resolveFunParam (ParamDecl pos ident (ParamTypeRefConst _ valType)) = pure (ident, absTypeToValType valType, ValConst, ValReference, pos)
resolveFunParam (ParamDecl pos ident (ParamTypeConstCopy _ valType)) = pure (ident, absTypeToValType valType, ValConst, ValCopy, pos)
resolveFunParam (ParamDecl pos ident (ParamTypeConstRef _ valType)) = pure (ident, absTypeToValType valType, ValConst, ValReference, pos)

checkCallParam :: BNFC'Position -> ((ValType, Maybe ValMutability), (ValType, ValMutability, ValPassingMechanism)) -> TypeCheckerM ()
checkCallParam pos ((valType, mMutability), (valType', mutability, passingMechanism)) = do
    checkCallParamType pos valType valType'
    checkCallParamMutability pos mMutability (mutability, passingMechanism)

checkCallParamType :: BNFC'Position -> ValType -> ValType -> TypeCheckerM ()
checkCallParamType pos valType valType' = do
    if valType == valType'
        then pure ()
        else throwError $ TypeCheckerException pos $ ExptFunParamWrongType valType valType'

checkCallParamMutability :: BNFC'Position -> Maybe ValMutability -> (ValMutability, ValPassingMechanism) -> TypeCheckerM ()
checkCallParamMutability pos Nothing (_, ValReference) = throwError $ TypeCheckerException pos ExptFunParamExprRefPass
checkCallParamMutability pos (Just ValConst) (ValMutable, ValReference) = throwError $ TypeCheckerException pos ExptFunParamConstMutablePass
checkCallParamMutability _ _ _ = pure ()
