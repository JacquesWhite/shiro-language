{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Src.Common.Environment where

import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Src.Shiro.Abs

class
    ( EnvT t ~ Map Ident t
    , ModEnv t ~ (EnvT t -> EnvT t)
    ) =>
    Env t
    where
    type EnvT t :: Type
    type EnvT t = Map Ident t

    type ModEnv t :: Type
    type ModEnv t = Map Ident t -> Map Ident t

    envEmpty :: EnvT t
    envEmpty = Map.empty

    envInsert :: [(Ident, t)] -> EnvT t -> EnvT t
    envInsert [] env = env
    envInsert ((ident, val) : xs) env = envInsert xs $ Map.insert ident val env

    envUnion :: EnvT t -> EnvT t -> EnvT t
    envUnion = Map.union

    envLookup :: Ident -> EnvT t -> Maybe t
    envLookup = Map.lookup
