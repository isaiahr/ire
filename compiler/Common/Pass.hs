{-- 
Pass.hs: machinery to run a pass, including associated logging tools

--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Common.Pass (runPass, writeMessages, messageNoLn, whitelistSev,
    createReportMsg, blacklistSev, Messages, Pass(..), byPassWith, (>>>>),
        (>>>=), Severity(..), PassResult(..), mkPassResult) where

import Common.Common
import Common.Reporting

import Control.Monad
import Control.Category
import Data.List

import System.CPUTime
import Control.DeepSeq
import GHC.Generics

messageNoLn pn s lvl = Messages [Left $ Message {mLine = Nothing, mPassName = pn, mStr = s, mSeverity = lvl}]

createReportMsg r = Messages [Right $ r]

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
          
blacklistSev (Messages msg) sevs = Messages (filter (\x -> case x of
                                                                Left x2 -> not (mSeverity x2 `elem` sevs)
                                                                Right x3 -> not (msgSeverity x3 `elem` sevs)) msg)
whitelistSev (Messages msg) sevs = Messages (filter (\x -> case x of
                                                                Left x2 -> (mSeverity x2 `elem` sevs)
                                                                Right x3 -> (msgSeverity x3 `elem` sevs)) msg)

newtype Messages = Messages [Either Message Report] deriving Eq

writeMessages (Messages m) = do
    forM m (\y -> do
        case y of
             Left m -> putStrLn $ disp m
             Right m -> writeReport m
        )
    return ()

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

data PassResult o = PassResult {
        prPassName :: String,
        prPassMessages :: Messages,
        prPassResult :: Maybe o,
        prPassFileInfo :: Maybe String,
        prTime :: [(String, Double)]
}

mkPassResult o fi = PassResult {
    prPassName = "-",
    prPassMessages = mempty,
    prPassResult = Just o,
    prPassFileInfo = fi,
    prTime = []
}

chfile :: Maybe String -> Messages -> Messages
chfile Nothing m = m
chfile (Just str) (Messages ((Left l):ms)) = (Messages [Left l]) <> (chfile (Just str) (Messages ms))
chfile (Just str) (Messages ((Right r):ms)) = (Messages [Right (r{msgFileName = Just str})]) <> (chfile (Just str) (Messages ms))
chfile (Just str) (Messages []) = Messages []


(>>>=) :: (Disp o, NFData o) => IO (PassResult i) -> (Pass i o) -> IO (PassResult o)
a >>>= b = (a >>= ( >>>> b))

(>>>>) :: (Disp o, NFData o) => (PassResult i) -> (Pass i o) -> IO (PassResult o)
a >>>> b = case prPassResult a of
                {-
                interesting fact: this is semantically identical to Nothing -> a
                but the typechecker doesnt like it because a is bound and not polymorphic,
                even though it actually doesnt contain any polymorphic data (the maybe is nothing).
                -}
                Nothing -> return PassResult {
                    prPassName = prPassName a,
                    prPassMessages = prPassMessages a,
                    prPassResult = Nothing,
                    prPassFileInfo = prPassFileInfo a,
                    prTime = []
                }
                Just o -> do
                    start <- getCPUTime
                    let (msg0, val) = (pFunc b) o
                    end <- deepseq val getCPUTime
                    let delta = end - start
                    let millis = (fromIntegral(delta)/1000000000)::Double
                    case (msg0, val) of
                               (msg, Nothing) -> return PassResult {
                                   prPassName = pName b,
                                   prPassMessages = (prPassMessages a) <> (chfile (prPassFileInfo a) msg),
                                   prPassResult = Nothing,
                                   prPassFileInfo = (prPassFileInfo a),
                                   prTime = (prTime a) <> [(pName b, millis)]
                               }
                               (msg, Just o') -> return PassResult {
                                   prPassName = pName b,
                                   prPassMessages = (prPassMessages a) <> messageNoLn (pName b) (disp o') Trees <> (chfile (prPassFileInfo a) msg),
                                   prPassResult = Just o',
                                   prPassFileInfo = (prPassFileInfo a),
                                   prTime = (prTime a) <> [(pName b, millis)]
                               }
    

-- a pass combinator. this can by used for parts of a previous pass to "bypass" a pass
-- this takes output of a pass, a, with a combinator a -> i, feeds result into the pass, and the output 
-- of the pass and the initial value into a final combinator.
byPassWith :: (a -> i) -> ((o, a) -> c) -> Pass i o -> Pass a c
byPassWith ic fc pass = Pass {pName = pName pass, pFunc = \a ->  case runPass (ic a) pass of
                                                                  (msg0, Nothing) -> (msg0, Nothing)
                                                                  (msg0, Just o) -> (msg0, Just (fc (o, a)))
                                                                  }
