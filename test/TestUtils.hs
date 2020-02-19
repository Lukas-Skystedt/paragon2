module TestUtils where
import Distribution.TestSuite as TS
import RunCommand
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import Control.Monad
import System.Console.GetOpt
import System.Console.ANSI

getAll :: FilePath -> IO [FilePath]
getAll g = do
  c <- getCurrentDirectory
  dirs <- getDirectoryContents (c </> g)
  files <- forM dirs $ \dir -> do
    f1 <- getDirectoryContents (c </> g </> dir)
    f2 <- fmap sort (filterM (\x -> return (".para" `isSuffixOf` x)) f1)
    mapM (\x -> return $ dir ++ "/" ++ x) f2
  return (concat files)

parac = "parac"

defaultLib = "lib"

runParac lib program =
  runCommandStrWait (parac ++ " --oldskool" ++ " -p " ++ lib ++ ": " ++ program) ""

runJavac :: String -> String -> IO (String,String,ExitCode)
runJavac lib program =
  runCommandStrWait ("javac -classpath '.:"++lib++"' "++program) ""

type StdOut = String
type StdErr = String

data ProgramRunResult = Success
                      | ParacErr StdOut StdErr ExitCode
                      | JavacErr StdOut StdErr ExitCode

showExitCode :: ExitCode -> String
showExitCode ExitSuccess = "0"
showExitCode (ExitFailure code) = show code

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

removePrefix :: FilePath -> String
removePrefix = reverse . drop 5 . takeWhile (/= '/') . reverse

startColor :: Color -> IO ()
startColor color = setSGR [SetColor Foreground Dull color]

resetColor :: IO ()
resetColor = setSGR [Reset]
