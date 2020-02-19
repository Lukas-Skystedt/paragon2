module SuiteGood where
import TestUtils as TU
import System.FilePath
import Distribution.TestSuite as TS
import System.Console.ANSI

tests :: IO [Test]
tests = do
  good <- TU.getAll "test/good"
  let paths = ("test/good/"++) <$> good
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
          actualOutput <- runTestCase lib program True
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
