module Src.TypeChecker.Types where

import Src.Common.Types
import Src.Shiro.Abs

absUnitTypeToValType :: TypeUnit -> ValType
absUnitTypeToValType (UnitType _) = ValTypeUnit

absTypeToValType :: TypeVal -> ValType
absTypeToValType (TypeInt _) = ValTypeInt
absTypeToValType (TypeBool _) = ValTypeBool
absTypeToValType (TypeString _) = ValTypeString
absTypeToValType (TypeFun _ params ret) =
    ValTypeFun (map absTypeToFunParam params) (absTypeToValType ret)
absTypeToValType (TypeFunUnitAuto _ params) =
    ValTypeFun (map absTypeToFunParam params) ValTypeUnit
absTypeToValType (TypeFunUnit _ params _) =
    ValTypeFun (map absTypeToFunParam params) ValTypeUnit

absTypeToFunParam :: ParamType -> FunParamType
absTypeToFunParam (ParamTypeMutableCopy _ t) =
    (absTypeToValType t, ValMutable, ValCopy)
absTypeToFunParam (ParamTypeConstCopy _ t) =
    (absTypeToValType t, ValConst, ValCopy)
absTypeToFunParam (ParamTypeMutableRef _ t) =
    (absTypeToValType t, ValMutable, ValReference)
absTypeToFunParam (ParamTypeRefConst _ t) =
    (absTypeToValType t, ValConst, ValReference)
absTypeToFunParam (ParamTypeConstRef _ t) =
    (absTypeToValType t, ValConst, ValReference)
