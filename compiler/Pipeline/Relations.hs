{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#include "../../build/config.h"

module Pipeline.Relations (importDag, UFile(..)) where

{--
Relations.hs: this handles the relationships between files.
-} 


import Parser.ParserRels
import Parser.Lexer

import Control.Monad.State
import System.Directory

import System.IO
import System.Exit
import System.FilePath
import Data.List

import Common.Common
import Common.Pass


-- a "unprocessed" file
data UFile = UFile {
    uPath :: String,
    uImports :: [String],
    uExports :: [String]
} deriving Show


data Ctx = Ctx {
    -- topological sorted files
    files :: [UFile]
}

-- returns the dag (top. sorted) in order the files should be proccessed.
importDag filename = do
    ctx <- execStateT (loadFiles [] filename) (Ctx { files = []})
    return (files ctx)

instance Disp [(Bool, String)] where
    disp a = error "TODO 584589455485498"
    
instance Disp [String] where
    disp a = error "TODO 9238489348934"
    
loadFiles :: [String] -> String -> StateT Ctx IO ()
loadFiles preds pth = do 
    -- canonicalizePath needed for seeing if two files are the same
    path <- liftIO $ canonicalizePath pth
    if path `elem` preds then
        liftIO $ ioError $ userError ("Imports form a cycle, in file " <> path)
                             else return ()
    inhandle <- liftIO $ openFile path ReadMode
    liftIO $ hSetEncoding inhandle utf8
    contents <- liftIO $ hGetContents inhandle
    u <- liftIO $ ((mkPassResult contents (Just path)) >>>> passLexer) >>>= passParseRels
    !ufile <- case (prPassResult u) of 
                   Nothing -> do
                       liftIO $ writeMessages (prPassMessages u)
                       liftIO $ exitWith (ExitFailure 1)
                   Just (im, ex) -> do
                       curdir <- liftIO $ getCurrentDirectory
                       liftIO $ setCurrentDirectory (takeDirectory path)
                       impaths <- forM im $ \(b, t0) -> liftIO $ canonicalizePath $ if b then STDLIB_PATH </> t0 <> ".ire" else t0
                       liftIO $ setCurrentDirectory curdir
                       return $ UFile {uPath = path, uImports = impaths, uExports = ex}
    liftIO $ hClose inhandle
    modify $ \ ctx -> ctx {files = rds (tsortInsert ufile preds (files ctx))}
    forM (uImports ufile) (loadFiles (path:preds))
    return ()

-- topological sort insert. 
-- this inserts a before the first occurence of anything in preds.
tsortInsert :: UFile -> [String] -> [UFile] -> [UFile]
tsortInsert a preds (l:ls) = if (uPath l) `elem` preds then (a:l:ls) else l:(tsortInsert a preds ls)
tsortInsert a preds [] = [a]
-- a has no predessors. this means we can put it at the end. (already happens, optimization)
-- tsortInsert a [] lst = lst ++ [a]

-- remove duplicates from tsort 
rds :: [UFile] -> [UFile]
rds (u:ufs) = u : rds (filter (\x -> (uPath x) /= (uPath u)) ufs)
rds [] = []


