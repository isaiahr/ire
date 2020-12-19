{-- 
Pass.hs: machinery to run a pass, including associated logging tools

--}
module Common.Pass (runPass, messageNoLn, messageLn, filterDbg, Pass(..), (>>>), Severity(..), arr) where

import Common.Common

import Control.Arrow
import Control.Category
import Data.List

data Severity = Error | Warning | Debug deriving Eq

shortS Error = "E"
shortS Warning = "W"
shortS Debug = "D"

longS Error = "Error"
longS Warning = "Warning"
longS Debug = "Debug"

messageNoLn pn s lvl = Messages [Message {mLine = Nothing, mPassName = pn, mStr = s, mSeverity = lvl}]
messageLn pn s lvl ln = Messages [Message {mLine = Just ln, mPassName = pn, mStr = s, mSeverity = lvl}]

runPass input pass = (pFunc pass) input

data Message = Message {
    mLine :: Maybe Int,
    mPassName :: String,
    mStr :: String,
    mSeverity :: Severity
}

instance Disp Message where
    disp message = header message <> (concat $ map (\x -> case x of
                                                        '\n' -> '\n' : (header message) 
                                                        x -> [x]) (mStr message))
header message = "[" <> (longS (mSeverity message)) <> "] [" <> mPassName message <> "] " <> ln
    where ln = case mLine message of
                    (Just ln) -> "<line " <> disp ln <> "> "
                    Nothing -> ""
          
filterDbg (Messages msg) = Messages (filter (\x -> mSeverity x /= Debug) msg)

newtype Messages = Messages [Message]

instance Disp Messages where
    disp (Messages m) = intercalate "\n" (map (disp) m)

instance Semigroup Messages where
    (Messages ms1) <> (Messages ms2) = Messages (ms1 <> ms2)

instance Monoid Messages where
    mempty = Messages []
    mappend = (<>)

-- a pass on an ast / lexstream / ir / etc
data Pass i o = Pass {
    pFunc  :: i -> (Messages, Maybe o),
    pName  :: [String]
}

instance Category Pass where
    id = Pass { pFunc = (\x -> (mempty, Just x)), pName = [] }
    f . g = Pass { pFunc = func, pName = pName f <> pName g }
        where func x = case (pFunc g) x of
                            (msg, Just o) -> case (pFunc f) o of
                                                  (msg2, oo) -> (msg <> msg2, oo)
                            (msg, Nothing) -> (msg, Nothing)



-- probably useless. 
instance Arrow Pass where
    arr f = Pass { pFunc = (\x -> (mempty, Just x)) Control.Category.. f, pName = [] }
    first pass = Pass { pFunc = fnc, pName = [] }
        where fnc = (\(a, c) -> case (pFunc pass) a of
                                     (msg, Just b) -> (msg, Just (b, c))
                                     (msg, Nothing) -> (msg, Nothing))


