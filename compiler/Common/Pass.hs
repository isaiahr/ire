{-- 
Pass.hs: machinery to run a pass, including associated logging tools

--}
module Common.Pass (runPass, messageNoLn, messageLn, filterDbg, filterErrs, Messages, Pass(..), byPassWith, (>>>>), Severity(..), PassResult(..), mkPassResult) where

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
} deriving Eq

instance Disp Message where
    disp message = header message <> (concat $ map (\x -> case x of
                                                        '\n' -> '\n' : (header message) 
                                                        x -> [x]) (mStr message))
header message = "[" <> (longS (mSeverity message)) <> "] [" <> mPassName message <> "] " <> ln
    where ln = case mLine message of
                    (Just ln) -> "<line " <> disp ln <> "> "
                    Nothing -> ""
          
filterDbg (Messages msg) = Messages (filter (\x -> mSeverity x /= Debug) msg)
filterErrs (Messages msg) = Messages (filter (\x -> mSeverity x == Error) msg)

newtype Messages = Messages [Message] deriving Eq

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
    pName  :: String
}

data PassResult o = PassOk String Messages o | PassFail String Messages

mkPassResult o = PassOk "-" mempty o

(>>>>) :: Disp o => PassResult i -> Pass i o -> PassResult o
a@(PassFail a1 a2) >>>> b = PassFail a1 a2

a@(PassOk str msg o) >>>> b = case ((pFunc b) o) of
    (msgs, Nothing) -> PassFail (pName b) (msg <> msgs)
    (msgs, Just dat) -> PassOk (pName b) (msg <> messageNoLn str (disp dat) Debug <> msgs) dat



-- a pass combinator. this can by used for parts of a previous pass to "bypass" a pass
-- this takes output of a pass, a, with a combinator a -> i, feeds result into the pass, and the output 
-- of the pass and the initial value into a final combinator.
byPassWith :: (a -> i) -> ((o, a) -> c) -> Pass i o -> Pass a c
byPassWith ic fc pass = Pass {pName = pName pass, pFunc = \a ->  case runPass (ic a) pass of
                                                                  (msg0, Nothing) -> (msg0, Nothing)
                                                                  (msg0, Just o) -> (msg0, Just (fc (o, a)))
                                                                  }
