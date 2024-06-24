import Src.Common.Exceptions
import Src.Common.Types
import Src.Shiro.Abs
import Src.TypeChecker.TypeChecker
import System.Directory.Internal.Prelude (exitFailure)

type TestCase = (String, TypeCheckerExceptionType)

testCases :: [TestCase]
testCases =
    [ ("02-op-on-undefined-val", ExptValNotDeclared (Ident "x"))
    , ("02-zero-div", ExptMathDivisionByZero)
    , ("03-assign-undefined-val", ExptValNotDeclared (Ident "x"))
    , ("03-bool-to-int-assignment", ExptModifyWrongType (Ident "x") ValTypeInt ValTypeBool)
    , ("03-bool-to-string-assignment", ExptModifyWrongType (Ident "x") ValTypeString ValTypeBool)
    , ("03-inline-redeclaration", ExptValAlreadyDeclared (Ident "x"))
    , ("03-wrong-type-val", ExptModifyWrongType (Ident "s") ValTypeString ValTypeInt)
    , ("03-inline-wrong-type-assignment", ExptModifyWrongType (Ident "x") ValTypeInt ValTypeString)
    , ("04-wrong-type-printInt-arg", ExptFunParamWrongType ValTypeString ValTypeInt)
    , ("05-wrong-type-condition", ExptWrongTypeCondExpr ValTypeInt)
    , ("06-09-val-in-fun", ExptValNotDeclared (Ident "x"))
    , ("06-modify-fun", ExptModifyFun (Ident "foo") (ValTypeFun [(ValTypeInt, ValMutable, ValCopy)] ValTypeInt))
    , ("07-08-modify-const-param", ExptModifyConst (Ident "x") ValTypeInt)
    , ("07-const-mutable-pass", ExptFunParamConstMutablePass)
    , ("08-modify-const", ExptModifyConst (Ident "x") ValTypeInt)
    , ("08-wrong-type-const", ExptModifyWrongType (Ident "s") ValTypeString ValTypeInt)
    , ("11-different-return-types", ExptFunReturnDifferentTypes ValTypeInt ValTypeString)
    , ("11-top-level-return-nested", ExptTopLevelProgramReturns)
    , ("11-top-level-return", ExptTopLevelProgramReturns)
    , ("11-wrong-return-type", ExptFunReturnWrongType (Ident "foo") ValTypeInt ValTypeString)
    , ("17-wrong-type-lambda", ExptModifyWrongType (Ident "f") (ValTypeFun [(ValTypeInt, ValConst, ValReference)] ValTypeInt) (ValTypeFun [(ValTypeInt, ValMutable, ValReference)] ValTypeInt))
    ]

testFile :: TestCase -> IO ()
testFile (fileName, expectedException) = do
    code <- readFile ("bad/" ++ fileName ++ ".shr")
    -- print code
    let testResult = testCode code
    case testResult of
        Left parserException -> do
            case parserException of
                TypeCheckerException _ actualException
                    | actualException == expectedException ->
                        putStrLn $ "Success " ++ "\"" ++ fileName ++ "\""
                _ -> do
                    putStrLn $ "TypeChecker test error in " ++ show fileName
                    putStrLn $ "Expected exception: " ++ show expectedException
                    putStrLn $ "Actual exception: " ++ show parserException
                    exitFailure
        Right _ -> do
            putStrLn $ "TypeChecker test error in " ++ show fileName
            putStrLn $ "Expected: " ++ show expectedException
            putStrLn "Got: No exception"
            exitFailure

testCode :: String -> ParserM ()
testCode code = do
    parserResult <- parseProgram code
    typeCheck parserResult

main :: IO ()
main = do
    putStrLn "Running tests..."
    mapM_ testFile testCases
