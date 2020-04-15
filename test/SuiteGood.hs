-- | Cabal test suite for tests cases that should be accepted by parac and
--  javac. The test cases are located in the 'good' directory. A test passes if
--  the output from parac and javac are both empty (stderr and stdout). This
--  comes from the previous test suite and may be problematic since not all
--  output necessarily signals an error.
module SuiteGood where
import TestUtils
import System.FilePath
import Distribution.TestSuite
import System.Console.ANSI

-- | Top level function of the 'good' test suite. Return a list of tests to be
-- run.
tests :: IO [Test]
tests = do
  removeOutput
  good <- getParaFiles $ "test" </> "good"
  let paths = (("test" </> "good") </>) <$> good
  let tests = makeTest defaultLib <$> paths
  return tests

-- | Convert a .para file path to a Test case (assuming the path belongs to the
-- 'good' test suite) using the provided library.
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
          -- Change last argument to True to run javac as well
          actualOutput <- runTestCase lib program False
          case actualOutput of
            Success -> return $ Finished Pass
            ParacErr stdOut stdErr exitCode -> do
              putStrLn "Output from parac:"
              startColor Red
              putStrLn (stdErr ++ stdOut)
              resetColor
              return $ Finished $ Fail $ "Parac returned an error, with ExitCode: " ++ showExitCode exitCode
            JavacErr stdOut stdErr exitCode -> do
              putStrLn "Output from javac:"
              startColor Red
              putStrLn (stdErr ++ stdOut)
              resetColor
              return $ Finished $ Fail $ "Javac returned an error, with ExitCode: " ++ showExitCode exitCode
