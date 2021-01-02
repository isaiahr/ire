{-# LANGUAGE CPP #-}

#include "../build/commitid.h"
module CLI.Main (main) where 

import System.Environment
import System.IO.Error
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.List
import Data.Maybe
import Control.Exception (evaluate)

import Common.Common
import Common.Pass

import Pipeline.Pipeline
import Pipeline.Target

data Options = Options {
    oDumptrees :: Bool,
    oInput     :: String,
    oOutput    :: String,
    oStage     :: Stage,
    oVersion   :: Bool,
    oHelp      :: Bool,
    oTarget    :: Target
} 

defaults = Options {
    oDumptrees = False,
    oInput = "",
    oOutput = "",
    oVersion = False,
    oStage = S_BIN,
    oHelp = False,
    oTarget = thisSystem
}


options = [
    Option ['v'] ["version"] (NoArg (\x -> x{oVersion = True})) "print version",
    Option ['h'] ["help"] (NoArg (\x -> x{oHelp = True})) "print this message",
    Option ['s'] ["stage"] (OptArg (\p x -> x{oStage = maybe S_BIN stageFromStr p}) "llvm/asm/obj/bin") "stop after stage, default binary",
    Option ['o'] ["output"] (ReqArg (\p x -> x{oOutput = p}) "file") "write output to file",
    Option ['t'] ["target"] (OptArg (\p x ->  x{oTarget = case (p >>= targetFromStr) of
                                                               Just t -> t
                                                               Nothing -> thisSystem} ) "os-arch") "compile for particular os-architecture pair, default this machine",
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
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    a <- getArgs
    pn <- getProgName
    op <- process a pn
    let filename = oInput op
    msgs <- pipelineIO (oTarget op) filename (oStage op) (oOutput op)
    let fmsg = if oDumptrees op then msgs else filterDbg msgs
    putStrLn $ disp fmsg
    return exitSuccess

    
process a pn = catchIOError (opts a pn) (\x -> hPutStrLn stderr (ioeGetErrorString x) >> exitFailure) >>= \y -> do
    if oOutput y == "" then
        if  ".ire" `isSuffixOf` (oInput y) then
            return (y { oOutput = (take ((length (oInput y)) - 4) (oInput y)) <> suffixOf (oStage y) })
                                        else do
            hPutStrLn stderr "Unsure where to write output. try specifying using -o"
            exitFailure
    else return y

