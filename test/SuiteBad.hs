module SuiteBad where
import TestUtils as TU
import System.FilePath
import Distribution.TestSuite as TS

tests :: IO [Test]
tests = do
  bad <- TU.getAll "test/bad"
  let paths = ("test/bad"++) <$> bad
  let tests = makeTest defaultLib <$> paths
  return tests
  where
    succeeds = TestInstance
        { run = return $ Finished Pass
        , name = "succeeds"
        , tags = []
        , TS.options = []
        , setOption = \_ _ -> Right succeeds
        }
    debug = TestInstance
         { run = return $ Finished Pass
         , name = "debug"
         , tags = []
         , TS.options = []
         , setOption = \_ _ -> Right debug
         }


makeTest lib program = Test testInst
  where testInst = TestInstance
          { run = return $ Finished Pass
          , name = program
          , tags = []
          , TS.options = []
          , setOption = \_ _ -> Right testInst
          }

getExpected :: FilePath -> IO String
getExpected program = do
  readFile (replaceExtension program "exp")
