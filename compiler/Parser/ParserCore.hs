module Parser.ParserCore (Parser(..), ParseResult(..), Reason(..), (@@), (<|>), (<*>), satisfy, parseEOF, collect, run, collectM, infbuild, parseToken)  where 

import Common.Common
import Parser.Lexer
import AST.AST
import Control.Applicative

-- result of running a parser on something.
-- typically this is either success, resulting in a parse tree (a) and resulting tokenstream 
-- or failure. (parsefailure). sometimes the error is unrecoverable, and this is when we disp
-- "syntax error" to user. in this case use unrecoverable.
data ParseResult a = ParseSuccess a [AnnotatedToken] | ParseFailure | Unrecoverable [Reason] deriving (Eq)

instance (Disp a) => Disp (ParseResult a) where
    disp (ParseSuccess b ts) = disp b
    disp ParseFailure = "Failure"
    disp (Unrecoverable r) = "unrecoverable" 
    
newtype Parser a = Parser ([AnnotatedToken] -> ParseResult a)

newtype Reason = Reason {message :: String} deriving (Show, Eq)

instance Disp Reason where
    disp r = message r

run :: Parser a -> [AnnotatedToken] -> ParseResult a
run (Parser ps) = ps

-- construct a parser to parse a token
-- this will not construct anything, so use it with (pure) parser to construct something.
satisfy :: (AnnotatedToken -> Bool) -> Parser ()
satisfy fun = Parser(\x -> 
    case x of
         (z:zs) -> if fun z then ParseSuccess () zs else ParseFailure
         _ -> ParseFailure)

(@@) :: AnnotatedToken -> Token -> Bool
ann @@ to = (annLexeme ann) == to

parseToken :: Token -> Parser ()
parseToken t = satisfy ( @@ t)

-- build new parser using a transformation to the output (func)
instance Functor Parser where
    fmap func (Parser p) = Parser (\x -> 
        case p x of
             ParseFailure -> ParseFailure
             ParseSuccess a ts -> ParseSuccess (func a) ts
             Unrecoverable r -> Unrecoverable r)

instance Applicative Parser where
    -- "identity" parser, construct a without changing tokenstream
    pure a = Parser (ParseSuccess a)
    -- parser p1 can construct b given a, p2 constructs a from ts
    -- p1 must run on ts then p2. 
    Parser p1 <*> Parser p2 = Parser (\ts -> 
        case p1 ts of -- run the first parser, to recover the function f
             ParseFailure -> ParseFailure
             Unrecoverable r -> Unrecoverable r
             ParseSuccess f ts2 -> 
                case p2 ts2 of -- run the second parser, and use result f(a)
                    ParseFailure -> ParseFailure
                    Unrecoverable r -> Unrecoverable r
                    ParseSuccess a ts3 -> ParseSuccess (f a) ts3)

instance Alternative Parser where
    empty = Parser $ const ParseFailure
    -- run p1, if failure, run p2
    Parser p1 <|> Parser p2 = Parser (\ts -> 
        case p1 ts of
             Unrecoverable r -> Unrecoverable r -- unrecoverable, dont bother 2nd
             ParseSuccess a ts2 -> ParseSuccess a ts2 -- success
             ParseFailure -> p2 ts) -- failure, return 2nd parser

instance Monad Parser where
    return = pure
    -- parser building a, and func given a returns parser building b
    Parser ps >>= f = Parser (\ts -> 
        case ps ts of 
             Unrecoverable r -> Unrecoverable r
             ParseFailure -> ParseFailure
             ParseSuccess a ts2 -> case f a of
                Parser p -> p ts2) -- desuger parser into parser func


-- collect: parses p1's seperated by sep into [p1]
collect :: Parser a1 -> Parser a2 -> Parser [a1]
collect p1 sep = Parser (\ts -> 
    case run p1 ts of
         ParseFailure -> ParseFailure
         Unrecoverable r -> Unrecoverable r
         ParseSuccess cn ts2 -> 
            case run (sep *> collect p1 sep) ts2 of
                ParseFailure -> ParseSuccess [cn] ts2
                Unrecoverable r2 -> Unrecoverable r2
                ParseSuccess cn2 ts3 -> ParseSuccess (cn:cn2) ts3)
                
-- collectM: (collect multiple) like collect, but allows multiple seperators
collectM :: Parser a1 -> Parser a2 -> Parser [a1]
collectM p1 sep = collect (pure ()) sep *> collect p1 (infbuild sep (const sep)) <* collect (pure ()) sep


-- like build, but will repeatedly apply build.
infbuild :: Parser a -> (a -> Parser a) -> Parser a
infbuild (Parser a) b = Parser (\ts ->
    -- try the original parser
    case a ts of
         ParseFailure -> ParseFailure
         Unrecoverable r -> Unrecoverable r
         ParseSuccess cn ts2 ->
         -- if original succeeds, we want to
         -- try the larger parser. 
            case run (b cn) ts2 of
                ParseFailure -> ParseSuccess cn ts2 -- this one fails, so previous is "largest"
                Unrecoverable r2 -> Unrecoverable r2
                -- this one succeeds, so we can keep building.
                -- run a new parser that will always parse the construct, and possibly construct larger result
                ParseSuccess cn2 ts3 -> run (infbuild (pure cn2) b) ts3)

-- parse end of file
parseEOF :: Parser ()
parseEOF = Parser (\ts -> case ts of
                              [] -> ParseSuccess () []
                              p ->  ParseFailure)
