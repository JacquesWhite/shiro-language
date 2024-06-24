{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Src.Evaluator.Environment where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Src.Common.Environment
import Src.Common.Exceptions
import Src.Shiro.Abs
import Src.Evaluator.Types
import Src.Evaluator.Store


envGet :: Ident -> EvaluatorEnv -> EvaluatorM Loc
envGet ident env = case envLookup ident env of
    Nothing -> throwError $ RuntimeException Nothing UnexpectedExceptionRuntime
    Just l -> pure l

addItemsToEnv :: [(Ident, Value)] -> EvaluatorM ModEvaluatorEnv
addItemsToEnv items = do
    locs <- getNewLocs $ length items
    mapM_ modStore $ zip items locs
    let idents = map fst items
    let envMod env = foldl modEnv env $ zip idents locs
    pure envMod
  where
    modStore :: ((x, Value), Loc) -> EvaluatorM a
    modStore ((_, value), loc) = do
        modify $ storeInsert [(loc, value)]
        pure undefined
    modEnv :: EvaluatorEnv -> (Ident, Loc) -> EvaluatorEnv
    modEnv env (ident, loc) = envInsert [(ident, loc)] env


getNewLocs :: Int -> EvaluatorM [Loc]
getNewLocs n = do
    s <- get
    let (l, s') = getNextLocs n s
    put s'
    pure l

getIdentVal :: Ident -> EvaluatorM Value
getIdentVal ident = do
    loc <- do
        env <- ask
        envGet ident env
    store <- get
    storeGet loc store

envGetValueAndLocFor :: Ident -> EvaluatorM (Value, Maybe Loc)
envGetValueAndLocFor ident = do
    loc <- envGet ident =<< ask
    store <- get
    val <- storeGet loc store
    pure (val, Just loc)
