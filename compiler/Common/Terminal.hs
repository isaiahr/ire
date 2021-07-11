{--

Common/Terminal.hs - utilities for interacting with the terminal. (colours etc)


-}

module Common.Terminal where

import System.IO

-- use canadian english because america is a failed state.
data Colour = Red | Green | Yellow


printColour col text = do
    let num = case col of
            Red -> "31"
            Green -> "32"
            Yellow -> "33"
    term <- hIsTerminalDevice stdout
    if term then
        putStr $ "\27[" <> num <> "m" <> text <> "\27[0m"
    else
        putStr text
            
