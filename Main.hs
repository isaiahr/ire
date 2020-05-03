{-# LANGUAGE CPP #-}

#include "build/commitid.h"
module Main (main) where 

import System.Environment
import System.IO.Error
import System.Console.GetOpt
import System.Exit
import Data.List
import Data.Maybe

import Common
import Pass
import Lexer
import Parser
import ParserCore
import AST
import Typer
import Namer
import NameTyper
import TypeChecker


data Options = Options {
    oDumptrees :: Bool,
    oInput     :: String,
    oOutput    :: Maybe String,
    oVersion   :: Bool,
    oHelp      :: Bool
} deriving Show

defaults = Options {
    oDumptrees = False,
    oInput = "",
    oOutput = Nothing,
    oVersion = False,
    oHelp = False
}


options = [
    Option ['v'] ["version"] (NoArg (\x -> x{oVersion = True})) "print version",
    Option ['h'] ["help"] (NoArg (\x -> x{oHelp = True})) "print this message",
    Option ['o'] ["output"] (OptArg (\p x -> x{oOutput = p}) "file") "write output to file",
    Option ['d'] ["dumptrees"] (NoArg (\x -> x{oDumptrees = True})) "write tokenstream, ast to stdout"]

opts ar pn = case getOpt Permute options ar of
                  (o, [n], []) -> norun (foldl (\x f -> f x) defaults o) msg >> return (foldl (\x f -> f x) defaults{oInput=n} o)
                  (o, _, er) -> norun (foldl (\x f -> f x) defaults o) msg >> ioError (userError (concat er ++ usageInfo msg options))
    where msg = "Usage: " ++ pn ++ " [options] file"


-- options that dont run the program.
norun opts msg = do
    if oVersion opts then
        putStrLn ("version " ++ VERSION_STRING ++ ", git commit " ++ COMMIT_ID) >> exitSuccess
                     else if oHelp opts then
                     putStrLn (usageInfo msg options) >> exitSuccess
                                        else return ()


main = do
    a <- getArgs
    pn <- getProgName
    op <- process a pn
    let filename = oInput op
    contents <- readFile filename 
    let transformations = passLexer >>>
                          passParse >>>
                          passName >>>
                          passType >>>
                          passTypeCheck
                   
    let (msg, result) = runPass contents transformations
    let fmsg = if oDumptrees op then msg else filterDbg msg
    putStrLn $ disp fmsg
    {-
    let result = lexFile contents
    if oDumptrees op then
        putStrLn $ intercalate "\n" (map show result)
                     else return ()
    let parsetree = run parseFile result
    case parsetree of
        ParseSuccess r ts -> do 
            if oDumptrees op then putStrLn (disp r) else return ()
            let r2 = name r
            if oDumptrees op then putStrLn (disp r2) else return ()
            let c = genConstraints r2
            if oDumptrees op then putStrLn (disp c) else return ()
            let solved = solve c
            if oDumptrees op then putStrLn (disp solved) else return ()
            let ast = nametypeAST solved r2
            if oDumptrees op then putStrLn (disp ast) else return ()
        ParseFailure -> putStrLn "failure parsing file"
        Unrecoverable r -> putStrLn ("failure parsing: " ++ (disp r))
        -}
    return exitSuccess

process a pn =  catchIOError (opts a pn) (\x -> putStrLn (ioeGetErrorString x) >> exitFailure) >>= return -- putStrLn . show

