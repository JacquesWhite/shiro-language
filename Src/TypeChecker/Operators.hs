{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

module Src.TypeChecker.Operators where

import Src.Shiro.Abs
import Src.Common.Types

data BiOperator
    = BiOpPlus
    | BiOpMinus
    | BiOpTimes
    | BiOpDiv
    | BiOpMod
    | BiOpLt
    | BiOpLe
    | BiOpGt
    | BiOpGe
    | BiOpEq
    | BiOpNe
    | BiOpAnd
    | BiOpOr
    deriving (Eq)

instance Show BiOperator where
    show BiOpPlus = "+"
    show BiOpMinus = "-"
    show BiOpTimes = "*"
    show BiOpDiv = "/"
    show BiOpMod = "%"
    show BiOpLt = "<"
    show BiOpLe = "<="
    show BiOpGt = ">"
    show BiOpGe = ">="
    show BiOpEq = "=="
    show BiOpNe = "!="
    show BiOpAnd = "&&"
    show BiOpOr = "||"

addOpToBiOperator :: AddOp -> BiOperator
addOpToBiOperator (OpPlus _) = BiOpPlus
addOpToBiOperator (OpMinus _) = BiOpMinus

mulOpToBiOperator :: MulOp -> BiOperator
mulOpToBiOperator (OpTimes _) = BiOpTimes
mulOpToBiOperator (OpDiv _) = BiOpDiv
mulOpToBiOperator (OpMod _) = BiOpMod

orOpToBiOperator :: OrOp -> BiOperator
orOpToBiOperator (OpOr _) = BiOpOr

andOpToBiOperator :: AndOp -> BiOperator
andOpToBiOperator (OpAnd _) = BiOpAnd

relOpToBiOperator :: RelOp -> BiOperator
relOpToBiOperator (OpLt _) = BiOpLt
relOpToBiOperator (OpLe _) = BiOpLe
relOpToBiOperator (OpGt _) = BiOpGt
relOpToBiOperator (OpGe _) = BiOpGe
relOpToBiOperator (OpEq _) = BiOpEq
relOpToBiOperator (OpNe _) = BiOpNe

biOpTypeFor :: BiOperator -> ((ValType, ValType) -> Maybe ValType)
biOpTypeFor BiOpPlus types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeInt
    (ValTypeString, ValTypeString) -> Just ValTypeString
    _ -> Nothing
biOpTypeFor BiOpMinus types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeInt
    _ -> Nothing
biOpTypeFor BiOpTimes types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeInt
    _ -> Nothing
biOpTypeFor BiOpDiv types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeInt
    _ -> Nothing
biOpTypeFor BiOpMod types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeInt
    _ -> Nothing
biOpTypeFor BiOpLt types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpLe types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpGt types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpGe types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpEq types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeBool
    (ValTypeBool, ValTypeBool) -> Just ValTypeBool
    (ValTypeString, ValTypeString) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpNe types = case types of
    (ValTypeInt, ValTypeInt) -> Just ValTypeBool
    (ValTypeBool, ValTypeBool) -> Just ValTypeBool
    (ValTypeString, ValTypeString) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpAnd types = case types of
    (ValTypeBool, ValTypeBool) -> Just ValTypeBool
    _ -> Nothing
biOpTypeFor BiOpOr types = case types of
    (ValTypeBool, ValTypeBool) -> Just ValTypeBool
    _ -> Nothing

data UnOperator
    = UnOpNeg
    | UnOpNot
    deriving (Eq)

instance Show UnOperator where
    show UnOpNeg = "-"
    show UnOpNot = "!"

opNegToBiOperator :: NegOp -> UnOperator
opNegToBiOperator (OpNeg _) = UnOpNeg

opNotToBiOperator :: NotOp -> UnOperator
opNotToBiOperator (OpNot _) = UnOpNot

unOpTypeFor :: UnOperator -> (ValType -> Maybe ValType)
unOpTypeFor UnOpNeg type' = case type' of
    ValTypeInt -> Just ValTypeInt
    _ -> Nothing
unOpTypeFor UnOpNot type' = case type' of
    ValTypeBool -> Just ValTypeBool
    _ -> Nothing
