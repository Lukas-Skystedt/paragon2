module TestUtils where
import Distribution.TestSuite as TS
import RunCommand
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import Control.Monad
import System.Console.GetOpt

color :: a -> String -> String
color _ = id

data Flag = JavaC
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option [] ["javac"] (NoArg JavaC) "Run javac compiler on generated .java files"
  ]

cmd c = do
  putStrLn c
  (out,err,code) <- runCommandStrWait c ""
  putStrLn out
  putStrLn err

getAll g = do
  c <- getCurrentDirectory
  dirs <- getDirectoryContents (c </> g)
  files <- forM dirs $ \dir -> do
    f1 <- getDirectoryContents (c </> g </> dir)
    f2 <- fmap sort (filterM (\x -> return (".para" `isSuffixOf` x)) f1)
    mapM (\x -> return $ dir ++ "/" ++ x) f2
  return (concat files)

getAllIssues = do
  pwd <- getCurrentDirectory
  isList <- getDirectoryContents (pwd </> "issues")
  filIssues <- filterM (\x -> return (isPrefixOf "issue" x)) isList
  return $ sort filIssues

isAsExpected str1 str2 = do
  if null str2 then do isEmpty str1 else
      if str1 == str2 then do
        putStrLn "ok"
        return 0
      else do
        putStrLn $ color 1 "Output does not match expected output"
        putStrLn $ color 1 "Expected output"
        putStrLn $ color 2 $ str2
        putStrLn $ color 1 "Received output"
        putStrLn $ color 3 $ str1
        return 1

isEmpty str = do
  if null str then do
        putStrLn "ok"
        return 0
    else do
        putStrLn $ color 1 "Output should be empty but isn't! Output was:"
        putStrLn $ color 3 $ str
        return 1

isNonEmpty str = do
  if null str then do
        putStrLn $ color 1 "Output should be non-empty but was!"
        return 1
    else do
        putStrLn "ok"
        return 0

dropLast n l = reverse (drop n (reverse l))

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


uglyErrorCheck e = do
  if e == "ExitSuccess" then do return 0 else do
    putStrLn $ color 1 "Error in compilation of parac."
    error "Test aborted."

--
-- My stuff below
--
parac = "./dist-newstyle/build/x86_64-linux/ghc-8.6.5/paragon-0.2.0/x/parac/build/parac/parac"

defaultLib = "lib"

runParac lib program =
  runCommandStrWait (parac ++ "-p " ++ lib ++ ": " ++ program) ""

runJavac :: String -> String -> IO (String,String,ExitCode)
runJavac lib program =
  runCommandStrWait ("javac -classpath '.:"++lib++"' "++program) ""

type StdOut = String
type StdErr = String

data ProgramRunResult = Success
                      | ParacErr StdOut StdErr ExitCode
                      | JavacErr StdOut StdErr ExitCode

runTestCase :: FilePath -> FilePath -> Bool -> IO ProgramRunResult
runTestCase lib program runJavaC = do
    (out,err,code) <- runParac lib program
    if null out && null err && code == 0
      then if runJavaC
             then do (jout,jerr,jcode) <- runJavac defaultLib (replaceExtension program "java")
                     if null jout && null jerr && jcode == 0
                       then return Success
                       else return $ JavacErr jout jerr jcode
             else return Success
      else return $ ParacErr out err code
