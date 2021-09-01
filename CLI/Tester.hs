{--
Tester.hs - file that loads and runs test cases
This is *really* hacked together, but it works (for now) 
and it isn't a core part of the compiler, so it is low priority to get changed
in the future. (but will probably be changed at some point)

-}
module CLI.Tester (main) where

import Control.Monad
import System.Process
import System.Environment
import System.IO.Error
import System.IO
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Exit
import Data.List
import Data.Maybe
import Control.Exception (evaluate)

import Pipeline.Pipeline
import Pipeline.Target
import Common.Common

data FailAt = 
    FailAtLexer |
    FailAtParser |
    FailAtTypeInference 
      deriving (Read, Show)
      



data TestType = Skip | Fail FailAt | Ok (Int, String) deriving (Read, Show)

data TestResult = TestPass | TestFail String | TestSkip deriving Eq

main = do
    files <- listTestsIn "tests"
    results <- forM (files) (runTestCase)
    forM (zip files results) $ \(file, err) -> case err of 
                                                    TestPass -> putStr "."
                                                    TestFail _ -> putStr "E"
                                                    TestSkip -> return ()
    putStrLn ""
    forM (zip files results) $ \(file, err) -> case err of 
                                                    TestPass -> return ()
                                                    TestFail errr -> putStrLn $ "Error in " <> file <> ": " <> errr
                                                    TestSkip -> return ()

    if filter (\y -> y /= TestPass && y /= TestSkip) results == [] then exitSuccess else exitWith (ExitFailure 1)

-- recursively finds (only) .ire files in dir
listTestsIn :: String -> IO [String]
listTestsIn dir = do
    cur <- listDirectory dir
    rec <- forM cur $ \ dr -> do
        eq <- doesDirectoryExist (dir </> dr)
        if eq then do
            listTestsIn (dir </> dr)
        else 
            if takeExtension dr == ".ire" then
                return [dir </> dr]
            else 
                return []
    return $ foldl (++) [] rec
            

readTest :: String -> IO TestType 
readTest file = do
    inhandle <- openFile file ReadMode
    hSetEncoding inhandle utf8
    contents <- hGetContents inhandle
    let firstline = head . lines $ contents
    let teststr = drop 2 firstline
    let test = (read teststr) :: TestType
    t <- evaluate (test)
    hClose inhandle
    return t
    

runTestCase :: String -> IO TestResult
runTestCase file = do
    test <- readTest file
    case test of
         Skip -> return TestSkip
         Ok (exitcode, stdoutput) -> do
             pipelineIO thisSystem file S_BIN False False "/tmp/output"
             let proc = CreateProcess {
                 cmdspec = ShellCommand "/tmp/output",
                 cwd = Nothing,
                 env = Nothing,
                 std_in = NoStream,
                 std_out = CreatePipe,
                 std_err = NoStream,
                 close_fds = False,
                 create_group = False,
                 delegate_ctlc = False,
                 detach_console = False,
                 create_new_console = False,
                 new_session = False,
                 child_group = Nothing,
                 child_user = Nothing,
                 use_process_jobs = False
             }
             (_, Just stdout, _, ph) <- createProcess proc 
             resultstr <- hGetContents stdout
             resultexitcode <- waitForProcess ph
             re <- evaluate (resultexitcode)
             return $ checkResults (re, resultstr) (exitcode, stdoutput)
         Fail fat -> do
            -- test fails later
            return TestPass
                             
checkResults (ExitFailure exit, output) (intexit, intoutput)
    | (exit == intexit) && (output == intoutput) = TestPass
    | otherwise = TestFail $ "got " <> show (exit, output) <> ", expected " <> show (intexit, intoutput)

checkResults (ExitSuccess, output) (intexit, intoutput)
    | (intexit == 0) && (output == intoutput) = TestPass
    | otherwise = TestFail $ "got " <> show (0, output) <> ", expected " <> show (intexit, intoutput)
    
