module Src.TypeChecker.StdLib where

import qualified Data.Map as Map
import Src.Common.StdLib
import Src.Common.Types
import Src.Shiro.Abs
import Src.TypeChecker.Environment

stdLib :: TypeCheckerEnv
stdLib = Map.fromList stdLibList
  where
    stdLibList :: [(Ident, ValProps)]
    stdLibList =
        map
            ( \stdFun ->
                let
                    fIdent = funIdent stdFun
                    fParams =
                        map
                            (\(_, pType, pMut, pPass) -> (pType, pMut, pPass))
                            (funParams stdFun)
                    fReturnType = funReturnType stdFun
                    fMut = ValConst
                 in
                    (fIdent, (ValTypeFun fParams fReturnType, fMut))
            )
            Src.Common.StdLib.stdLib
