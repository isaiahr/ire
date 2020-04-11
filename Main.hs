module Main (main) where 

import System.Environment
import System.Exit
import Data.List

import Lexer
import Parser
import ParserCore
import AST
import Typer



main = getArgs >>= process >> exitSuccess


process :: [String] -> IO ()
process ["-h"] = putStrLn "Usage: [file]"
process (file:files) = do
    contents <- readFile file
    let result = lexFile contents
    putStrLn $ intercalate "\n" (map show result)
    let b = run parseFile result
    case b of
         ParseSuccess r ts -> do 
             putStrLn (disp r)
             print $ typeAST r
         ParseFailure -> putStrLn "failure"
         Unrecoverable r -> putStrLn "big failure"
    
process _ =  return ()

displ :: Show a => [a] -> String
displ (l:ls) = show l ++ "\n" ++ displ ls
displ [] = ""
