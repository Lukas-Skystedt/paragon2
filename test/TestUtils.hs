-- | This module contains utility functions used by the test suites.
module TestUtils where
import Distribution.TestSuite
import RunCommand
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import Control.Monad
import System.Console.GetOpt
import System.Console.ANSI

-- | Given a directory, returns a list of the relative paths of all .para files
-- in that directory recursively.
getParaFiles :: FilePath -> IO [FilePath]
getParaFiles g = do
  c <- getCurrentDirectory
  dirs <- getDirectoryContents (c </> g)
  files <- forM dirs $ \dir -> do
    f1 <- getDirectoryContents (c </> g </> dir)
    f2 <- fmap sort (filterM (\x -> return (".para" `isSuffixOf` x)) f1)
    mapM (\x -> return $ dir ++ "/" ++ x) f2
  return (concat files)

-- | The command used to execute the compiler
parac :: String
parac = "parac"

-- | Path to the library files passed to parac
defaultLib :: FilePath
defaultLib = "test" </> "lib"

appendOutputDir :: FilePath -> FilePath
appendOutputDir path = outputDir </> path

outputDir :: FilePath
outputDir = "test" </> "output"

-- | Run parac with the given library and .para file
runParac :: FilePath -> FilePath -> IO (String, String, ExitCode)
runParac lib program =
  runCommandStrWait (parac ++ " --oldskool" ++ " -p " ++ lib ++ " -o " ++ outputDir ++ " " ++ program) ""

-- | Run javac with the given classpath and .java file
runJavac :: FilePath -> FilePath -> IO (String, String, ExitCode)
runJavac lib program =
  runCommandStrWait ("javac -classpath '.:"++lib++"' "++ appendOutputDir program) ""

type StdOut = String
type StdErr = String

-- | Represents the result of running parac and then javac on the output.
data ProgramRunResult = Success
                      | ParacErr StdOut StdErr ExitCode
                      | JavacErr StdOut StdErr ExitCode

-- | Convert ExitCode to an integer as a String
showExitCode :: ExitCode -> String
showExitCode ExitSuccess = "0"
showExitCode (ExitFailure code) = show code

-- | Run a test using the provided library and .para paths. If the
-- runJavaC flag is set to True, also run javac on the output from parac.
-- Success is only returned if the outputs from parac (and javac) are empty.
runTestCase :: FilePath -> FilePath -> Bool -> IO ProgramRunResult
runTestCase lib program runJavaC = do
    (out,err,code) <- runParac lib program
    case (out,err,code) of
      -- Success is only returned if stdOut and stdErr are empty
      ("", "", ExitSuccess)
        | not runJavaC -> return Success
        | runJavaC -> do
          (jout,jerr,jcode) <- runJavac defaultLib (replaceExtension program "java")
          case (jout, jerr, jcode) of
            ("", "", ExitSuccess) -> return Success
            _ -> return $ JavacErr jout jerr jcode
      _ -> return $ ParacErr out err code

-- | Start coloring terminal output text.
startColor :: Color -> IO ()
startColor color = setSGR [SetColor Foreground Dull color]

-- | Reset coloring for terminal output.
resetColor :: IO ()
resetColor = setSGR [Reset]
