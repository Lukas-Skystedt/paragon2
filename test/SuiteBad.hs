module SuiteBad where
import TestUtils as TU
import System.FilePath
import Distribution.TestSuite as TS
import System.Console.ANSI

tests :: IO [Test]
tests = do
  bad <- TU.getAll "test/bad"
  let paths = ("test/bad/"++) <$> bad
  let tests = makeTest defaultLib <$> paths
  return tests

makeTest :: FilePath -> FilePath -> Test
makeTest lib program = Test testInst
  where testInst = TestInstance
          { run = runTest
          , name = removePrefix program
          , tags = []
          , TS.options = []
          , setOption = \_ _ -> Right testInst
          }
        runTest = do
          expectedOutput <- getExpected program
          actualOutput <- runTestCase lib program False
          case actualOutput of
            Success -> return $ Finished $ Fail "Parac compiled program containing errors"
            ParacErr stdOut stdErr _exitCode
             | (stdErr ++ stdOut) == expectedOutput -> return $ Finished Pass
             | otherwise -> do
                putStrLn "Exptected output:"
                startColor Green
                putStrLn expectedOutput
                resetColor
                putStrLn "Actual output:"
                startColor Red
                putStrLn (stdErr ++ stdOut)
                resetColor
                return $ Finished $ Fail "Expected output does not match actual output."
            JavacErr {} -> return $ Finished $ Error "Javac should not be called for bad test cases"

getExpected :: FilePath -> IO String
getExpected program =
  readFile (replaceExtension program "exp")
