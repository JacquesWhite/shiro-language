module Src.Common.StdLib where
import Src.Shiro.Abs
import Src.Common.Types
import Src.Evaluator.Types
import Src.Common.Environment

data StdFun = StdFun
    { funIdent :: Ident
    , funParams :: [(Ident, ValType, ValMutability, ValPassingMechanism)]
    , funReturnType :: ValType
    , funBody :: FunBlock
    , funEnv :: EvaluatorEnv
    }

stdLib :: [StdFun]
stdLib = 
    [ StdFun (Ident "printInt") [(Ident "i", ValTypeInt, ValConst, ValCopy)] ValTypeUnit PrintInt envEmpty
    , StdFun (Ident "printString") [(Ident "s", ValTypeString, ValConst, ValCopy)] ValTypeUnit PrintString envEmpty
    , StdFun (Ident "printBool") [(Ident "b", ValTypeBool, ValConst, ValCopy)] ValTypeUnit PrintBool envEmpty
    , StdFun (Ident "exit") [(Ident "n", ValTypeInt, ValConst, ValCopy)] ValTypeUnit Exit envEmpty
    ]
