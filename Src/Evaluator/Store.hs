module Src.Evaluator.Store where
import Src.Evaluator.Types
import qualified Data.Map as Map
import Src.Common.Exceptions
import Control.Monad.Except



type ModStore = Store -> Store

storeEmpty :: Store
storeEmpty = Store 0 Map.empty

getNextLocs :: Int -> Store -> ([Loc], Store)
getNextLocs n (Store nextLoc mem) = (newLocs, newStore)
  where
    newLocs = [nextLoc .. nextLoc + n - 1]
    newStore = Store (nextLoc + n) mem

storeInsert :: [(Loc, Value)] -> Store -> Store
storeInsert [] store = store
storeInsert [(loc, value)] (Store nextLoc mem) = Store nextLoc (Map.insert loc value mem)
storeInsert ((loc, value) : rest) store = storeInsert rest $ storeInsert [(loc, value)] store

storeGet :: Loc -> Store -> EvaluatorM Value
storeGet loc (Store _ mem) = case Map.lookup loc mem of
    Nothing -> throwError $ RuntimeException Nothing UnexpectedExceptionRuntime
    Just value -> pure value
