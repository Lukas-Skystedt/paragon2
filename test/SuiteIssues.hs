-- | Cabal test suite for regressions tests. The test cases are located in the
-- 'issues' directory.
module SuiteIssues where
import TestUtils as TU
import System.FilePath
import System.Directory
import Distribution.TestSuite as TS
import Data.List

-- | Top level function of the 'issues' test suite. Return a list of tests to be
-- run.
tests :: IO [Test]
tests = do
  issues <- getAllIssues
  print issues
  return []

-- | Convert a .para file path to a Test case (assuming the path belongs to the
-- 'issues' test suite) using the provided library.
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
                putStrLn expectedOutput
                putStrLn "Actual output:"
                putStrLn (stdErr ++ stdOut)
                return $ Finished $ Fail "Expected output does not match actual output."
            JavacErr {} -> return $ Finished $ Error "Javac should not be called for bad test cases"

-- | Read the contents of file with expected test output from the file
-- corresponding to the given .para file.
getExpected :: FilePath -> IO String
getExpected program =
  readFile (replaceExtension program "exp")

-- | Get a list of all Issue folders.
getAllIssues :: IO [FilePath]
getAllIssues = do
  pwd <- getCurrentDirectory
  print pwd
  isList <- getDirectoryContents (pwd </> "test/issues")
  print isList
  let filIssues = filter ("issue" `isPrefixOf`) isList
  return $ sort filIssues
