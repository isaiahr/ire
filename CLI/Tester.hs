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
import System.Directory
import System.Console.GetOpt
import System.Exit
import Data.List
import Data.Maybe
import Control.Exception (evaluate)

import Common.Target
import Common.Common
import Common.Pass
import Parser.Lexer
import Parser.Parser
import AST.AST
import Pass.YieldInjection
import Pass.Typer
import Pass.Namer
import Pass.NameTyper
import Pass.TypeChecker
import IR.DirectCall
import IR.Lowering
import IR.HeapConversion
import IR.LambdaLift
import IR.CodeGen


data FailAt = 
    FailAtLexer |
    FailAtParser |
    FailAtTypeInference 
      deriving (Read, Show)
      

runOk contents = snd $ runPass contents allpasses
    where allpasses =     passLexer >>> -- plaintext -> tokens
                          passParse >>> -- tokens -> ast<string>
                          passYieldInj >>> -- ast<string> -> ast<string>
                          passName >>> -- ast<string> -> ast<name>
                          passType >>> -- ast<name> -> ast<typedname>
                          passTypeCheck >>> -- ast<typedname> -> ast<typedname>, ensures type annotation correctness
                          passLower >>>  -- ast<typedname> -> IR
                          passDCall >>> -- IR -> IR, direct call conversion
                          passHConv >>> -- IR -> IR, promote freevars to heap 
                          passLLift >>> -- IR -> IR, lift nested functions to top level
                          passGenLLVM

doTrans :: TestType -> String -> Maybe String
doTrans (Right _) c = Nothing  -- handled later

doTrans (Left FailAtLexer) contents = case (runPass contents passLexer) of
                                           (msg, Nothing) -> Nothing
                                           (msg, Just p) -> Just "Expected failure, but pass produced value"
doTrans (Left FailAtParser) c = case (runPass c stage1) of
                                     (msg, Nothing) -> Just "Expected success, but failed passes"
                                     (msg, Just p) -> case (runPass p stage2) of
                                                           (msg2, Nothing) -> Nothing
                                                           (msg2, Just p3) -> Just "Expected failure, but pass produced value"
    where stage1 = passLexer
          stage2 = passParse

doTrans (Left FailAtTypeInference) c = case (runPass c stage1) of
                                            (msg, Nothing) -> Just "Expected success, but failed passes"
                                            (msg, Just p) -> case (runPass p stage2) of
                                                                (msg2, Nothing) -> Nothing
                                                                (msg2, Just p3) -> Just "Expected failure, but pass produced value"
    where stage1 = passLexer >>> passParse >>> passYieldInj >>> passName
          stage2 = passType


type TestType = Either FailAt (Int, String)


main = do
    files <- listDirectory "tests"
    results <- forM (map ("tests/" <> ) files) runTestCase
    forM (zip files results) $ \(file, err) -> case err of 
                                                      Nothing -> putStr "."
                                                      Just errr -> do
                                                          putStr "E"
    putStrLn ""
    forM (zip files results) $ \(file, err) -> case err of 
                                                      Nothing -> return ()
                                                      Just errr -> do
                                                          putStrLn $ "Error in " <> file <> ": " <> errr
    if filter (/=Nothing) results == [] then exitSuccess else exitWith (ExitFailure 1)
    
runTestCase :: String -> IO (Maybe String)
runTestCase file = do
    inhandle <- openFile file ReadMode
    hSetEncoding inhandle utf8
    contents <- hGetContents inhandle
    let firstline = head . lines $ contents
    let teststr = drop 2 firstline
    let test = (read teststr) :: TestType
    contents_sz <- evaluate (length contents)
    hClose inhandle
    let msg = doTrans test contents
    case msg of
         Just p  -> return $ Just p
         Nothing -> case test of
                         Left failat -> return $ Nothing
                         Right (exitcode, stdoutput) -> do
                             case runOk contents of
                                  Nothing -> return $ Just "Should have passed, but failed"
                                  Just res -> do
                                      writeOutput (disp res) "/tmp/output" thisSystem S_BIN
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
                             
checkResults (ExitFailure exit, output) (intexit, intoutput)
    | (exit == intexit) && (output == intoutput) = Nothing
    | otherwise = Just $ "got " <> show (exit, output) <> ", expected " <> show (intexit, intoutput)

checkResults (ExitSuccess, output) (intexit, intoutput)
    | (intexit == 0) && (output == intoutput) = Nothing
    | otherwise = Just $ "got " <> show (0, output) <> ", expected " <> show (intexit, intoutput)
    
