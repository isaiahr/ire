module Main (main) where 
import System.Environment
import System.Exit
import Lexer
import Parser

main = getArgs >>= process >> exitSuccess


process :: [String] -> IO ()
process ["-h"] = putStrLn "Usage: [file]"
process (file:files) = do
    contents <- readFile file
    let result = lexFile contents
    putStrLn $ disp result
    let b = run parseFile result
    print b
    
process _ =  return ()

disp :: Show a => [a] -> String
disp (l:ls) = show l ++ "\n" ++ disp ls
disp [] = ""
