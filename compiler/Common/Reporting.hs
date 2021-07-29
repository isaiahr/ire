{-
Common/Reporting.hs - provides tools to format messages to the terminal
                                     to report errors / warnings effectively


-}

module Common.Reporting (Report(..), writeReport, Severity(..), longS, shortS) where

import Common.Terminal
import System.IO
import Control.Monad
import Data.List

-- line no, start col, end (line no, end col)
type MsgSpan = (Int, Int, Int, Int)

data Severity = Error | Warning | Debug | Trees deriving Eq

shortS Error = "E"
shortS Warning = "W"
shortS Debug = "D"
shortS Trees = "T"

longS Error = "Error"
longS Warning = "Warning"
longS Debug = "Debug"
longS Trees = "Trees"

data Report = Report {
    msgMainText :: String,
    msgExcerpt :: String,
    msgAnnotations :: [(MsgSpan, String)],
    msgFileName :: Maybe String,
    msgNote :: [String],
    msgSeverity :: Severity,
    msgErrorCode :: Int,
    msgPassName :: String
} deriving Eq

writeReport :: Report -> IO ()
writeReport msgo = do
    msg <- case msgFileName msgo of
                Just fn -> do 
                    con' <- readFile fn
                    return $ msgo {msgExcerpt = con'}
                Nothing -> return msgo
    let colr = case msgSeverity msg of
                 Error -> Red
                 Warning -> Yellow
    printColour colr $ (longS $ msgSeverity msg) <> "[" <> (shortS $ msgSeverity msg) <> (show $ msgErrorCode msg) <> "]"
    putStrLn $ ": " <> (msgMainText msg)
    case msgFileName msg of
         Just filename -> do
             putStr $ "in File: "
             printColour Blue (filename <> " ")
             case msgAnnotations msg of 
                  (((ls, cs, le, ce), _):_) -> do
                      printColour Purple $ "line " <> show ls <> ", col " <> show cs <> "\n"
                  _ -> putStrLn ""
         Nothing -> do
             putStrLn $ "(no location)"
    let ln = lines2 (msgExcerpt msg)
    _<- forM (zip [1..(length ln)] ln) (\(lineno, line) -> do
        case (filter (inSpan lineno) (msgAnnotations msg)) of
             [] -> return () -- skip
             [(a, str)] -> do
                 writeLine a lineno line str)
    return ()
        
        
inSpan lineno ((lns, _, lne, _), _) = lineno >= lns && lineno <= lne


writeLine (linestart, colstart, lineend, colend) lineno line text = do
    let linew = length (show lineno)
    let spacer = ((replicateM linew " ") !! 0)
    printColour Purple $ (show lineno) <> " | "
    printColour Red $ line <> "\n"
    let prefix = if (linestart == lineno) then colstart-1 else 0
    let postfix = if (lineend == lineno) then (colend-colstart)+1 else ((length line)-colstart)
    let pointer = ((replicateM prefix " ") !! 0) <> ((replicateM postfix "^") !! 0)
    printColour Purple $ spacer <> " | "
    printColour Red $ pointer <> "\n"
    printColour Purple $ spacer <> " | "
    printColour Red $ ((replicateM prefix " ") !! 0) <> text <> "\n"

-- like lines, but no undesirable behavoir where a\n\n\nb gets split into ["a", "b"]
lines2 :: String -> [String]
lines2 str = case (elemIndex '\n' str) of
                  Nothing -> [str]
                  (Just idx) -> case (splitAt idx str) of
                                  (a, '\n':b) -> [a] <> (lines2 b)
