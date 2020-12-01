{-# LANGUAGE CPP #-}

#include "build/commitid.h"
module Main (main) where 

import System.Environment
import System.IO.Error
import System.Console.GetOpt
import System.Exit
import Data.List
import Data.Maybe

import Common.Common
import Common.Pass
import Parser.Lexer
import Parser.Parser
import AST.AST
import Pass.Typer
import Pass.Namer
import Pass.NameTyper
import Pass.TypeChecker
import IR.DirectCall
import IR.Lowering
import IR.HeapConversion
import IR.LambdaLift
import IR.CodeGen

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
    Option ['d'] ["dumptrees"] (NoArg (\x -> x{oDumptrees = True})) "write trees to stdout"]

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
    let transformations = passLexer >>> -- plaintext -> tokens
                          passParse >>> -- tokens -> ast<string>
                          passName >>> -- ast<string> -> ast<name>
                          passType >>> -- ast<name> -> ast<typedname>
                          passTypeCheck >>> -- ast<typedname> -> ast<typedname>, ensures type annotation correctness
                          passLower >>>  -- ast<typedname> -> IR
                          passDCall >>> -- IR -> IR, direct call conversion
                          passHConv >>> -- IR -> IR, promote freevars to heap 
                          passLLift >>> -- IR -> IR, lift nested functions to top level
                          passGenLLVM -- IR -> LLVM
                          
                   
    let (msg, result) = runPass contents transformations
    let fmsg = if oDumptrees op then msg else filterDbg msg
    putStrLn $ disp fmsg
    return exitSuccess

process a pn =  catchIOError (opts a pn) (\x -> putStrLn (ioeGetErrorString x) >> exitFailure) >>= return

