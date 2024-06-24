{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Src.TypeChecker.Environment where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Src.Common.Environment
import Src.Common.Exceptions
import Src.Common.Types
import Src.Shiro.Abs

type TypeCheckerM a = ExceptT ExceptionHolder (State TypeCheckerEnv) a

instance Env ValProps

type TypeCheckerEnv = EnvT ValProps
type ModTypeCheckerEnv = ModEnv ValProps

modifyEnvForFunction ::
    [(Ident, ValProps)] ->
    [FunParamWithIdentAndPos] ->
    TypeCheckerEnv ->
    TypeCheckerEnv
modifyEnvForFunction f params = envUnion newEnv
  where
    fEnv = Map.fromList f
    vEnv = Map.fromList $ map (\(ident, valType, mutability, _, _) -> (ident, (valType, mutability))) params
    newEnv = envUnion fEnv vEnv

localState :: ModTypeCheckerEnv -> TypeCheckerM a -> TypeCheckerM a
localState environmentModifier checkComputation = do
    currentEnv <- get
    put (environmentModifier currentEnv)
    result <- checkComputation
    put currentEnv
    return result
