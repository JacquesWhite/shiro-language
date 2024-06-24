module Src.Evaluator.StdLib where

import Src.Common.Environment
import Src.Common.StdLib
import Src.Evaluator.Store
import Src.Evaluator.Types

makeStdLib :: EvaluatorEnv -> Store -> (EvaluatorEnv, Store)
makeStdLib env store =
    let
        stdLibCount = length stdLib
        (locs, store') = getNextLocs stdLibCount store
        namesAndValues =
            map
                ( \stdFun ->
                    ( funIdent stdFun
                    , ValFun
                        ( map
                            ( \(ident, _, _, valPassingMechanism) ->
                                (ident, valPassingMechanism)
                            )
                            (funParams stdFun)
                        )
                        (funBody stdFun)
                        (funEnv stdFun)
                    )
                )
                stdLib
        (names, values) = unzip namesAndValues
        env' = envInsert (zip names locs) env
        store'' = storeInsert (zip locs values) store'
     in
        (env', store'')
