module SuiteIssues where
import TestUtils as TU
import System.FilePath
import System.Directory
import Distribution.TestSuite as TS
import Data.List

tests :: IO [Test]
tests = do
  print "hello"
  issues <- getAllIssues
  print issues
  -- let paths = ("test/bad/"++) <$> issues
  -- let tests = makeTest defaultLib <$> paths
  return []
  -- return tests

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
                putStrLn expectedOutput
                putStrLn "Actual output:"
                putStrLn (stdErr ++ stdOut)
                return $ Finished $ Fail "Expected output does not match actual output."
            JavacErr {} -> return $ Finished $ Error "Javac should not be called for bad test cases"

getExpected :: FilePath -> IO String
getExpected program =
  readFile (replaceExtension program "exp")

getAllIssues :: IO [FilePath]
getAllIssues = do
  pwd <- getCurrentDirectory
  print pwd
  isList <- getDirectoryContents (pwd </> "test/issues")
  print isList
  let filIssues = filter ("issue" `isPrefixOf`) isList
  return $ sort filIssues
