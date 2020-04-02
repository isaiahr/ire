module ParserCore (Parser(..), ParseResult(..), Reason(..), (@@), (<|>), (<*>), satisfy)  where 

import Lexer
import Control.Applicative

-- result of running a parser on something.
-- typically this is either success, resulting in a parse tree (a) and resulting tokenstream 
-- or failure. (parsefailure). sometimes the error is unrecoverable, and this is when we disp
-- "syntax error" to user. in this case use unrecoverable.
data ParseResult a = ParseSuccess a [AnnotatedToken] | ParseFailure | Unrecoverable [Reason] deriving (Show, Eq)
newtype Parser a = Parser ([AnnotatedToken] -> ParseResult a)

data Reason = Reason {message :: String} deriving (Show, Eq)

-- construct a parser to parse a token
-- this will not construct anything, so use it with (pure) parser to construct something.
satisfy fun = Parser(\x -> 
    case x of
         (z:zs) -> if (fun z) then ParseSuccess () zs else ParseFailure
         _ -> ParseFailure)

(@@) :: AnnotatedToken -> Token -> Bool
(AnnotatedToken t l str) @@ to = (t == to)

-- build new parser using a transformation to the output (func)
instance Functor Parser where
    fmap func (Parser p) = Parser (\x -> 
        case (p x) of
             ParseFailure -> ParseFailure
             ParseSuccess a ts -> ParseSuccess (func a) ts
             Unrecoverable r -> Unrecoverable r)

instance Applicative Parser where
    -- "identity" parser, construct a without changing tokenstream
    pure a = Parser (\ts -> ParseSuccess a ts)
    -- parser p1 can construct b given a, p2 constructs a from ts
    -- p1 must run on ts then p2. 
    Parser p1 <*> Parser p2 = Parser (\ts -> 
        case (p1 ts) of -- run the first parser, to recover the function f
             ParseFailure -> ParseFailure
             Unrecoverable r -> Unrecoverable r
             ParseSuccess f ts2 -> 
                case (p2 ts2) of -- run the second parser, and use result f(a)
                    ParseFailure -> ParseFailure
                    Unrecoverable r -> Unrecoverable r
                    ParseSuccess a ts3 -> ParseSuccess (f a) ts3)

instance Alternative Parser where
    empty = Parser (\x -> ParseFailure)
    -- run p1, if failure, run p2
    Parser p1 <|> Parser p2 = Parser (\ts -> 
        case (p1 ts) of
             Unrecoverable r -> Unrecoverable r -- unrecoverable, dont bother 2nd
             ParseSuccess a ts2 -> ParseSuccess a ts2 -- success
             ParseFailure -> p2 ts) -- failure, return 2nd parser

instance Monad Parser where
    return = pure
    -- parser building a, and func given a returns parser building b
    Parser ps >>= f = Parser (\ts -> 
        case (ps ts) of 
             Unrecoverable r -> Unrecoverable r
             ParseFailure -> ParseFailure
             ParseSuccess a ts2 -> case (f a) of
                Parser p -> p ts2) -- desuger parser into parser func
