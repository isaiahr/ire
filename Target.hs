{-# LANGUAGE CPP #-}

#include "build/config.h"

module Target where

import System.IO
import System.Process
import System.Directory
import System.Exit
import Data.Time.Clock.POSIX

import Common.Common
{-
Target.hs -- module specifying targets, which are os / architecture pairs,
 and platform specific tools to be run after compilation (linker, assembler, etc)
-}

-- see http://llvm.org/doxygen/Triple_8h_source.html

data OpSystem = Linux | MacOS | Windows deriving Eq

instance Disp OpSystem where 
    disp Linux = "linux"
    disp MacOS = "macos"
    disp Windows = "windows"


data Arch = AMD64 | I386 | AArch64 | AArch32 deriving Eq

instance Disp Arch where
    disp AMD64 = "amd64"
    disp I386 = "i386"
    disp AArch64 = "aarch64"
    disp AArch32 = "aarch32"

data Target = Target OpSystem Arch deriving Eq

instance Disp Target where
    disp (Target os arch) = disp os <> "-" <> disp arch
    
data Stage = S_LLVM | S_ASM | S_OBJ | S_BIN deriving Eq

instance Disp Stage where
    disp S_LLVM = "llvm"
    disp S_ASM = "asm"
    disp S_OBJ = "obj"
    disp S_BIN = "bin"

stageFromStr "llvm" = S_LLVM
stageFromStr "asm" = S_ASM
stageFromStr "obj" = S_OBJ
stageFromStr "bin" = S_BIN
-- default: bin
stageFromStr other = S_BIN

suffixOf S_LLVM = ".ll"
suffixOf S_ASM = ".s"
suffixOf S_OBJ = ".o"
suffixOf S_BIN = ""

allOSes = [Linux, MacOS, Windows]
allArches = [AMD64, I386, AArch64, AArch32]

allTargets = [Target] <*>  allOSes <*> allArches

thisSystem = case targetFromStr HOST_SYSTEM of
                  Just s -> s
                  Nothing -> error "compiler improperly configured"

targetFromStr str = case (filter (\(x, y) -> y == str) (map (\x -> (x, disp x)) allTargets)) of
                         [(t, s)] -> Just t
                         otherwise -> Nothing


getTempFile = do
    time <- getPOSIXTime
    return $ "/tmp/iretmp" <> show time
    
    
-- get the other libs to pass to linker 
getLinkedLibs (Target Linux AMD64) = return [LINUX_AMD64_LIB_PATH]
    
-- ok, it will exit. 
getLinkedLibs t = targetUnsupported t

runLinker :: Target -> [String] -> String -> IO ()
runLinker (Target Linux _) paths output = do
    handle <- runProcess LINUX_LINKER_PATH (paths <> ["-o", output]) Nothing Nothing Nothing Nothing Nothing
    result <- waitForProcess handle
    return ()
    
    
runLinker t@(Target _ _) filepath output = targetUnsupported t


runLLC :: Target -> String -> String ->  IO ()
runLLC target path output = do
    handle <- runProcess LLC_PATH [path, "-o", output] Nothing Nothing Nothing Nothing Nothing
    result <- waitForProcess handle
    return ()

-- llc with assembler, to emit obj.
runLLCAsm :: Target -> String -> String -> IO ()
runLLCAsm target path output = do
    handle <- runProcess LLC_PATH [path, "--filetype=obj",  "-o", output] Nothing Nothing Nothing Nothing Nothing
    result <- waitForProcess handle
    return ()

targetUnsupported t = do
    hPutStrLn stderr $ "target " <> disp t <> "unsupported"
    exitFailure

runCompiler :: String -> String -> IO ()
runCompiler output outfile = do
    handle <- openFile outfile WriteMode
    hSetEncoding handle utf8
    hPutStr handle output
    hClose handle
    
writeOutput :: String -> String -> Target -> Stage -> IO ()
writeOutput output ofile target stage = do
    case stage of
         S_LLVM -> runCompiler output ofile
         S_ASM -> do
             lfile <- getTempFile
             runCompiler output lfile
             runLLC target lfile ofile 
             removeFile lfile
         S_OBJ -> do
             lfile <- getTempFile
             runCompiler output lfile
             runLLCAsm target lfile ofile 
             removeFile lfile
         S_BIN -> do
             lfile <- getTempFile
             objfile <- getTempFile
             runCompiler output lfile
             runLLCAsm target lfile objfile
             libs <- getLinkedLibs target  
             runLinker target (libs <> [objfile]) ofile
             removeFile lfile
             removeFile objfile
