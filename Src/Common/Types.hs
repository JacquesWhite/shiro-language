module Src.Common.Types where
import Src.Shiro.Abs

data ValMutability
  = ValMutable
  | ValConst
  deriving (Eq)

instance Show ValMutability where
    show ValMutable = "mutable"
    show ValConst = "const"


data ValPassingMechanism
  = ValReference
  | ValCopy
  deriving (Eq)


instance Show ValPassingMechanism where
  show ValReference = "reference"
  show ValCopy = "copy"


data ValType
    = ValTypeUnit
    | ValTypeInt
    | ValTypeBool
    | ValTypeString
    | ValTypeFun [FunParamType] ValType
    deriving (Eq)

instance Show ValType where
    show ValTypeUnit = "Unit"
    show ValTypeInt = "Int"
    show ValTypeBool = "Bool"
    show ValTypeString = "String"
    show (ValTypeFun params ret) = "(" ++ show params ++ ") -> " ++ show ret

type ValProps = (ValType, ValMutability)

type FunParamType = (ValType, ValMutability, ValPassingMechanism)

type FunParamWithIdentAndPos = (Ident, ValType, ValMutability, ValPassingMechanism, BNFC'Position)

funParamWithoutIdentAndPos :: FunParamWithIdentAndPos -> FunParamType
funParamWithoutIdentAndPos (_, t, m, p, _) = (t, m, p)
