module Parser (parseType, run, Type(..)) where

import ParserCore
import Lexer
import Control.Applicative

-- [t], t, or t -> t
data Type = Array Type | AtomicType AtomicType | Function Type Type deriving (Show, Eq)

-- Int
data AtomicType = IntType deriving (Show, Eq)

-- runs parser on tokenstream
run (Parser ps) ts = ps ts

--build (Parser p1) (Parser p2) = (\x -> )

-- parses a token of specific type.
parseToken t = satisfy (\x -> x @@ t)

-- for avoiding left recursion.
-- parses the non left recursive left token, 
-- then "builds" b using constructed a. if b cannot be constructed, a is returned
build :: Parser a -> (a -> Parser a) -> Parser a
build (Parser a) b = Parser (\ts ->
    case (a ts) of
         ParseFailure -> ParseFailure
         Unrecoverable r -> Unrecoverable r
         ParseSuccess construct ts2 -> -- ok. try 2nd
            case (run (b construct) ts2) of 
                 ParseFailure -> ParseFailure -- ParseSuccess construct ts2
                 Unrecoverable r2 -> Unrecoverable r2
                 ParseSuccess nc ts3 -> ParseSuccess nc ts3)

-- like build, but will repeatedly apply build.
infbuild :: Parser a -> (a -> Parser a) -> Parser a
infbuild (Parser a) b = Parser (\ts ->
    -- try the original parser
    case (a ts) of
         ParseFailure -> ParseFailure
         Unrecoverable r -> Unrecoverable r
         ParseSuccess cn ts2 ->
         -- if original succeeds, we want to
         -- try the larger parser. 
            case (run (b cn) ts2) of
                ParseFailure -> ParseSuccess cn ts2 -- this one fails, so previous is "largest"
                Unrecoverable r2 -> Unrecoverable r2
                -- this one succeeds, so we can keep building.
                -- run a new parser that will always parse the construct, and possibly construct larger result
                ParseSuccess cn2 ts3 -> run (infbuild (pure cn2) b) ts3)
                                    

parseType :: Parser Type
parseType = infbuild (parseIntType <|> parseArrayType) parseFunctionType

parseIntType :: Parser Type
parseIntType = parseToken (Identifier "Int") *> pure (AtomicType IntType)

parseArrayType = parseToken (LSqParen) *> (fmap Array parseType) <* parseToken (RSqParen)

-- parseFunctionType = liftA2 Function parseType (parseToken (Arrow) *> parseType)
parseFunctionType t = liftA2 Function (pure t) (parseToken (Arrow) *> parseType)
