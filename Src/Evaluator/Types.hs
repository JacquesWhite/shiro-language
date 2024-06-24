{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Src.Evaluator.Types where
import Control.Monad.Except
import Src.Common.Exceptions
import Control.Monad.Reader
import Src.Common.Environment
import Control.Monad.State
import qualified Data.Map as Map
import Src.Shiro.Abs
import Src.Common.Types


type Loc = Int

instance Env Loc
type EvaluatorEnv = EnvT Loc
type ModEvaluatorEnv = ModEnv Loc

data Store = Store
    { nextFree :: Loc
    , memory :: Map.Map Loc Value
    }

type EvaluatorM a = ExceptT ExceptionHolder (ReaderT EvaluatorEnv (StateT Store IO)) a

data Value
    = ValInt Int
    | ValBool Bool
    | ValString String
    | ValUnit
    | ValFun [FunParam] FunBlock EvaluatorEnv

instance Show Value where
    show :: Value -> String
    show (ValInt val) = show val
    show (ValBool val) = show val
    show (ValString val) = show val
    show ValUnit = "Unit"
    show (ValFun params _ _) = "fun(" ++ show params ++ ")"

data FunBlock
    = UserDefined Block
    | PrintString
    | PrintInt
    | PrintBool
    | Exit

type FunParam = (Ident, ValPassingMechanism)

type RetType = Maybe Value
