module Parser (parseFile, run, Type(..)) where

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
data AtomicType = Bits Int deriving (Eq)

instance Show AtomicType where
    show (Bits n) = "bits" ++ show n

data Definition = Definition {identifier::[Char],  typeof::Type, value::Expression} deriving (Eq)

instance Show Definition where
    show def = (identifier def) ++ show (typeof def) ++ show (value def)
    showList (ds) = \x -> (intercalate "\n" $ map show ds) ++ x

data Expression = Literal Literal | FunctionCall Expression Expression deriving (Show, Eq)

-- a literal
data Literal = Constant Int | ArrayLiteral [Expression] | TupleLiteral [Expression] | RecordLiteral [([Char], Expression)] | FunctionLiteral String Body deriving (Eq)

instance Show Literal where
    show (Constant i) = show i
    show (ArrayLiteral e) = "[" ++ intercalate ", " (map show e) ++ "]"
    show (TupleLiteral t) = "(" ++ intercalate ", " (map show t) ++ ")"
    show (RecordLiteral r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ " = " ++ show y) r) ++ "}"
    show (FunctionLiteral p b) = '\\' : p ++ " -> {\n" ++ show b ++ "}"

data Body = Body [Statement] deriving (Show, Eq)

data Statement = Defn Definition | Expr Expression deriving (Show, Eq)
-- runs parser on tokenstream
run (Parser ps) ts = ps ts


parseFile = collectM parseDefinition $ parseToken Term

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
parseIntType = parseToken (Identifier "Int") *> pure (AtomicType (Bits 64))

parseArrayType = parseToken (LSqParen) *> (fmap Array parseType) <* parseToken (RSqParen)

parseFunctionType t = liftA2 Function (pure t) (parseToken (Arrow) *> parseType)

parseTuple = fmap Tuple (parseToken (LParen) *> collect parseType (parseToken Comma) <* parseToken (RParen))

parseRecord = fmap Record (parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseType)) (parseToken Comma) <* parseToken RCrParen)

-- future optimization: roll parseRecord and parseUnion into one func
parseUnion = fmap Union (parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseType)) (parseToken Pipe) <* parseToken RCrParen)

parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parseIdentifier (parseToken Colon *> parseType) (parseToken Equals *> parseExpression)


parseExpression = infbuild (parseBrExpression <|> parseLiteral) parseFunctionCall

-- parse bracketed expression
parseBrExpression = parseToken LParen *> parseExpression <* parseToken RParen

parseLiteral = fmap Literal $ parseInt <|> parseArrayLiteral <|> parseTupleLiteral <|> parseRecordLiteral  <|> parseFunctionLiteral

parseInt = fmap Constant $ Parser (\x -> 
    case x of
         (AnnotatedToken (Integer z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)

parseArrayLiteral = fmap ArrayLiteral $ parseToken LSqParen *> collect parseExpression (parseToken Comma) <* parseToken RSqParen

-- change this so (expr) is parseerror
parseTupleLiteral = fmap ArrayLiteral $ parseToken LParen *> collect parseExpression (parseToken Comma) <* parseToken RParen

parseRecordLiteral = fmap RecordLiteral $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Equals *> parseExpression)) (parseToken Comma) <* parseToken RCrParen

-- \a -> {}
parseFunctionLiteral = liftA2 FunctionLiteral (parseToken BSlash *> parseIdentifier <* parseToken Arrow) (parseToken LCrParen *> parseBody <* parseToken RCrParen)

parseBody = fmap Body (collectM parseStatement $ parseToken Term)

parseStatement = fmap Defn parseDefinition <|> fmap Expr parseExpression
-- UnionLiteral (([Char], Expression), [([Char], Type)])
-- remove union literals for now. not completely sure on syntax for them yet
-- parseUnionLiteral = fmap UnionLiteral $ parseToken LCrParen *> liftA2 (\x y -> (x, y)) () () <* parseToken RCrParen


parseFunctionCall t = liftA2 FunctionCall (pure t) parseExpression
