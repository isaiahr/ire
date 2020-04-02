module Main (main) where 
import System.Environment
import System.Exit
import Lexer
import Parser

main = getArgs >>= process >> exitWith ExitSuccess

process ["-h"] = putStrLn "Usage: [file]"
process (file:files) = do
    contents <- readFile file
    let result = lexFile contents
    putStrLn (disp result)
    let b = run parseType result
    putStrLn (show b)
    
process _ =  return ()

disp (l:ls) = show l ++ "\n" ++ disp ls
disp [] = ""
