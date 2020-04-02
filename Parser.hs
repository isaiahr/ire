module Parser (parseType, run, Type(..)) where

import ParserCore
import Lexer
import Control.Applicative
import Data.List

{--
 TYPE SYSTEM
 types fall into 6 categories:
 category         | example         | example value
 natives          | int             | 5
 tuples           | (int, string)   | (3, "a")
 arrays           | [int]           | [3,4,5,2]
 function         | int -> int      | \x -> x * 2
 record           | {x:Int, y:Int}  | {x=3, y=2}
 union            | {x:Int | y:Int} | {x=3 | y:Int} 
--}
-- [t], t, t -> t, (t1, t2, ...), {a:t1, b:t2, ...}
data Type = Array Type | AtomicType AtomicType | Function Type Type | Tuple [Type] | Record [([Char], Type)] | Union [([Char], Type)]
    deriving (Eq)

instance Show Type where 
    show (Array t) = "[" ++ show t ++ "]"
    show (AtomicType t) = show t
    show (Function f t) = show f ++ " -> " ++ show t
    show (Tuple arr) = "(" ++ (intercalate ", " (map show arr)) ++ ")"
    show (Record r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ": " ++ show y) r) ++ "}"
    show (Union u) = "{" ++ intercalate " | " (map (\(x, y) -> x ++ ": " ++ show y) u) ++ "}"
-- Int
data AtomicType = Bits Int Bool  deriving (Eq)

instance Show AtomicType where
    show (Bits n True) = "int" ++ show n
    show (Bits n False) = "uint" ++ show n

-- runs parser on tokenstream
run (Parser ps) ts = ps ts

-- parses an identifier
parseIdentifier :: Parser [Char]
parseIdentifier = Parser (\x -> 
    case x of 
         (AnnotatedToken (Identifier z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)         

parseType :: Parser Type
parseType = parseBType <|> (infbuild (parseRecord <|> parseUnion <|> parseIntType <|> parseArrayType <|> parseTuple) parseFunctionType)

-- bracketed type
parseBType = parseToken (LParen) *> parseType <* parseToken (RParen)

parseIntType :: Parser Type
parseIntType = parseToken (Identifier "Int") *> pure (AtomicType (Bits 64 True))

parseArrayType = parseToken (LSqParen) *> (fmap Array parseType) <* parseToken (RSqParen)

parseFunctionType t = liftA2 Function (pure t) (parseToken (Arrow) *> parseType)

parseTuple = fmap Tuple (parseToken (LParen) *> collect parseType (parseToken Comma) <* parseToken (RParen))

parseRecord = fmap Record (parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseType)) (parseToken Comma) <* parseToken RCrParen)

-- future optimization: roll parseRecord and parseUnion into one func
parseUnion = fmap Union (parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseType)) (parseToken Pipe) <* parseToken RCrParen)
