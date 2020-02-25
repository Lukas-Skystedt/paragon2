-- | Cabal test suite for regressions tests. The test cases are located in the
-- 'issues' directory.
module SuiteIssues where
import TestUtils as TU
import System.FilePath
import System.Directory
import Distribution.TestSuite as TS
import Data.List
import Control.Monad
import System.Console.ANSI

-- | Top level function of the 'issues' test suite. Return a list of tests to be
-- run.
tests :: IO [Test]
tests = do
  removeOutput
  issues <- getAllIssues
  let tests = makeTest defaultLib <$> issues
  return tests

-- | Convert a .para file path to a Test case (assuming the path belongs to the
-- 'issues' test suite) using the provided library.
makeTest :: FilePath -> FilePath -> Test
makeTest lib issue = Test testInst
  where testInst = TestInstance
          { run = runTest
          -- Use the name of the .para file as test name
          , name = takeBaseName issue
          , tags = []
          , options = []
          , setOption = \_ _ -> Right testInst
          }
        runTest = do
          expectedOutput <- getExpected issue
          filesToCompile <- getIssueParaFiles issue
          totalOut <- forM filesToCompile $ \file -> do
            let runJavaC = null expectedOutput
            actualOutput <- runTestCase lib file runJavaC
            case actualOutput of
              Success -> return ""
              ParacErr stdOut stdErr _exitCode -> return (stdErr ++ stdOut)
              JavacErr stdOut stdErr _exitCode -> return (stdErr ++ stdOut)
          let concattedTotalOut = concat totalOut
          if concattedTotalOut == expectedOutput then return $ Finished Pass else do
            putStrLn "Exptected output:"
            startColor Green
            putStrLn expectedOutput
            resetColor
            putStrLn "Actual output:"
            startColor Red
            putStrLn concattedTotalOut
            resetColor
            return $ Finished $ Fail "Expected output doesn't match actual output"

-- | Read the contents of file with expected test output from the file
-- corresponding to the given .para file.
getExpected :: FilePath -> IO String
getExpected issue = readFile $ issue </> "expected"

-- | Get paragon files to compile
getIssueParaFiles :: FilePath -> IO [String]
getIssueParaFiles issue = do
  content <- readFile $ issue </> "compile"
  pwd <- getCurrentDirectory
  let paths = ((pwd </> issue) </>) <$> lines content
  return paths

-- | Get a list of all Issue folders.
getAllIssues :: IO [FilePath]
getAllIssues = do
  pwd <- getCurrentDirectory
  isList <- listDirectory (pwd </> "test" </> "issues")
  let filIssues = sort $ filter ("issue" `isPrefixOf`) isList
  let paths = (("test" </> "issues") </>) <$> filIssues
  return paths
