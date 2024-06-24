module Main where

import System.IO
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)

import Src.Common.Exceptions (
    ExceptionHolder (ControlledExit),
 )
import Src.Evaluator.Evaluator (evalProgram)
import Src.Shiro.Abs
import Src.TypeChecker.TypeChecker (ParserM, parseProgram, typeCheck)

preProcessCode :: String -> ParserM Program
preProcessCode code = do
    program <- parseProgram code
    let typeCheckerResult = typeCheck program
    case typeCheckerResult of
        Left parserException -> Left parserException
        Right _ -> Right program

ignoreR :: a -> [Char]
ignoreR _ = ""

run :: String -> IO ()
run code = do
    let preProcessResult = preProcessCode code
    case preProcessResult of
        Left parserException -> do
            hPutStr stderr $ show parserException
            exitFailure
        Right program -> do
            evaluatorResult <- evalProgram program
            hPutStr stderr . either show ignoreR $ evaluatorResult
            case evaluatorResult of
                Left (ControlledExit 0) -> exitSuccess
                Left (ControlledExit statusCode) -> exitWith $ ExitFailure statusCode
                Left _ -> exitFailure
                Right () -> pure ()

printUsage :: IO ()
printUsage = do
    hPutStr stderr $
        unlines
            [ "USAGE:"
            , "    ./interpreter [FILE]"
            , "    ./interpreter OPTIONS"
            , ""
            , "OPTIONS:"
            , "    -h, --help"
            , "        Display this help message."
            , ""
            , "ARGUMENTS:"
            , "    <FILE>"
            , "        File to interpret. Use no argument to read from standard input."
            , ""
            ]

main :: IO ()
main = do
    params <- getArgs
    case params of
        ["--help"] -> printUsage
        [] -> getContents >>= run
        [f] -> readFile f >>= run
        _ -> printUsage
