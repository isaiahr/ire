module Parser (parseFile, run, Type(..)) where

import ParserCore
import Lexer
import Control.Applicative
import Data.List
import Data.Functor

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
data Type = Array Type | AtomicType AtomicType | Function Type Type | Tuple [Type] | Record [(String, Type)] | Union [(String, Type)]
    deriving (Eq)

instance Show Type where 
    show (Array t) = "[" ++ show t ++ "]"
    show (AtomicType t) = show t
    show (Function f t) = show f ++ " -> " ++ show t
    show (Tuple arr) = "(" ++ intercalate ", " (map show arr) ++ ")"
    show (Record r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ ": " ++ show y) r) ++ "}"
    show (Union u) = "{" ++ intercalate " | " (map (\(x, y) -> x ++ ": " ++ show y) u) ++ "}"
-- Int
newtype AtomicType = Bits Int deriving (Eq)

instance Show AtomicType where
    show (Bits n) = "bits" ++ show n

data Definition = Definition {identifier :: String,  typeof :: Maybe Type, value :: Expression} deriving (Eq)

instance Show Definition where
    show def = identifier def ++ ": " ++ shw (typeof def) ++ " = " ++ show (value def)
        where shw (Just a) = show a
              shw Nothing = ""

data Expression = Literal Literal | FunctionCall Expression Expression deriving (Eq)

instance Show Expression where
    show (Literal l) = show l
    show (FunctionCall e1 e2) = show e1 ++ show e2

-- a literal
data Literal = Constant Int | ArrayLiteral [Expression] | TupleLiteral [Expression] | RecordLiteral [(String, Expression)] | FunctionLiteral String Body deriving (Eq)

instance Show Literal where
    show (Constant i) = show i
    show (ArrayLiteral e) = "[" ++ intercalate ", " (map show e) ++ "]"
    show (TupleLiteral t) = "(" ++ intercalate ", " (map show t) ++ ")"
    show (RecordLiteral r) = "{" ++ intercalate ", " (map (\(x, y) -> x ++ " = " ++ show y) r) ++ "}"
    show (FunctionLiteral p b) = '\\' : p ++ " -> {\n" ++ show b ++ "\n}"

newtype Body = Body [Statement] deriving (Eq)

instance Show Body where
    show (Body s) = intercalate "\n" (map show s)

data Statement = Defn Definition | Expr Expression | Assignment String Expression deriving (Eq)

instance Show Statement where
    show (Defn s) = show s
    show (Expr e) = show e
    show (Assignment ident e) = ident ++ " = " ++ show e

newtype ParseTree = ParseTree [Definition] deriving (Eq)

instance Show ParseTree where
    show (ParseTree (d:ds)) = show d ++ "\n" ++ show (ParseTree ds)
    show _ = ""

-- runs parser on tokenstream
run :: Parser p -> [AnnotatedToken] -> ParseResult p
run (Parser ps) = ps

parseFile :: Parser ParseTree
parseFile = fmap ParseTree $ collectM parseDefinition $ parseToken Term

-- parses an identifier
parseIdentifier :: Parser String
parseIdentifier = Parser (\x -> 
    case x of
         (AnnotatedToken (Identifier z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)         

parseType :: Parser Type
parseType = parseBType <|> infbuild (parseRecord <|> parseUnion <|> parseIntType <|> parseArrayType <|> parseTuple) parseFunctionType

-- bracketed type
parseBType :: Parser Type
parseBType = parseToken LParen *> parseType <* parseToken RParen

parseIntType :: Parser Type
parseIntType = parseToken (Identifier "Int") $> AtomicType (Bits 64)

parseArrayType :: Parser Type
parseArrayType = parseToken LSqParen *> fmap Array parseType <* parseToken RSqParen

parseFunctionType :: Type -> Parser Type
parseFunctionType t = liftA2 Function (pure t) (parseToken Arrow *> parseType)

parseTuple :: Parser Type
parseTuple = fmap Tuple $ parseToken LParen *> collect parseType (parseToken Comma) <* parseToken RParen

parseRecord :: Parser Type
parseRecord = fmap Record $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseType)) (parseToken Comma) <* parseToken RCrParen

-- future optimization: roll parseRecord and parseUnion into one func
parseUnion :: Parser Type
parseUnion = fmap Union $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseType)) (parseToken Pipe) <* parseToken RCrParen

parseDefinition :: Parser Definition
parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parseIdentifier ((parseToken Colon *> fmap Just parseType) <|> (parseToken Colon $> Nothing)) (parseToken Equals *> parseExpression)

parseExpression :: Parser Expression
parseExpression = infbuild (parseBrExpression <|> parseLiteral) parseFunctionCall

-- parse bracketed expression
parseBrExpression :: Parser Expression
parseBrExpression = parseToken LParen *> parseExpression <* parseToken RParen

parseLiteral :: Parser Expression
parseLiteral = fmap Literal $ parseInt <|> parseArrayLiteral <|> parseTupleLiteral <|> parseRecordLiteral  <|> parseFunctionLiteral

parseInt :: Parser Literal
parseInt = Constant <$> Parser (\x -> 
    case x of
         (AnnotatedToken (Integer z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)

parseArrayLiteral :: Parser Literal
parseArrayLiteral = fmap ArrayLiteral $ parseToken LSqParen *> collect parseExpression (parseToken Comma) <* parseToken RSqParen

-- change this so (expr) is parseerror
parseTupleLiteral :: Parser Literal
parseTupleLiteral = fmap ArrayLiteral $ parseToken LParen *> collect parseExpression (parseToken Comma) <* parseToken RParen

parseRecordLiteral :: Parser Literal
parseRecordLiteral = fmap RecordLiteral $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Equals *> parseExpression)) (parseToken Comma) <* parseToken RCrParen

-- \a -> {}
parseFunctionLiteral :: Parser Literal
parseFunctionLiteral = liftA2 FunctionLiteral (parseToken BSlash *> parseIdentifier <* parseToken Arrow) (parseToken LCrParen *> parseBody <* parseToken RCrParen)

parseBody :: Parser Body
parseBody = fmap Body (collectM parseStatement $ parseToken Term)

parseStatement :: Parser Statement
parseStatement = fmap Defn parseDefinition <|> parseAssignment <|> fmap Expr parseExpression

parseAssignment :: Parser Statement
parseAssignment = liftA2 Assignment (parseIdentifier <* parseToken Equals) parseExpression
-- UnionLiteral ((String, Expression), [(String, Type)])
-- remove union literals for now. not completely sure on syntax for them yet
-- parseUnionLiteral = fmap UnionLiteral $ parseToken LCrParen *> liftA2 (\x y -> (x, y)) () () <* parseToken RCrParen

parseFunctionCall :: Expression -> Parser Expression
parseFunctionCall t = liftA2 FunctionCall (pure t) parseExpression
