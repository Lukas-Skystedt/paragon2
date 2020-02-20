-- | Cabal test suite for tests that where errors are expected from parac. The
-- | test cases are located in the 'bad' directory. Each .para file is a test
-- | case and for each test case there is also a file containing the expected
-- | compiler output (error message).
-- |
-- | The test suite is not updated for current versions of error messages and
-- | thus this suite passes the '--oldskool' flag to get the old messages.
module SuiteBad where
import TestUtils as TU
import System.FilePath
import Distribution.TestSuite as TS
import System.Console.ANSI

-- | Top level function of the 'bad' test suite. Return a list of tests to be
-- | run.
tests :: IO [Test]
tests = do
  bad <- getParaFiles "test/bad"
  let paths = ("test/bad/"++) <$> bad
  let tests = makeTest defaultLib <$> paths
  return tests

-- | Convert a .para file path to a Test case (assuming the path belongs to the
-- | 'bad' test suite) using the provided library.
makeTest :: FilePath -> FilePath -> Test
makeTest lib program = Test testInst
  where testInst = TestInstance
          { run = runTest
          -- Use the name of the .para file as test name
          , name = takeBaseName program
          , tags = []
          , options = []
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

-- | Read the contents of file with expected test output from the file
-- | corresponding to the given .para file.
getExpected :: FilePath -> IO String
getExpected program =
  readFile (replaceExtension program "exp")
