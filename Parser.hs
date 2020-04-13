module Parser (parseFile, run, Type(..)) where

import ParserCore
import AST
import Lexer
import Control.Applicative
import Data.List
import Data.Functor


-- runs parser on tokenstream
run :: Parser p -> [AnnotatedToken] -> ParseResult p
run (Parser ps) = ps

parseFile :: Parser (AST String)
parseFile = fmap AST $ collectM parseDefinition $ parseToken Term

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

parseDefinition :: Parser (Definition String)
parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parseIdentifier ((parseToken Colon *> fmap Just parseType) <|> (parseToken Colon $> Nothing)) (parseToken Equals *> parseExpression)

parseExpression :: Parser (Expression String)
parseExpression = infbuild (parseBrExpression <|> parseLiteral <|> parseVariable) parseFunctionCall

parseVariable :: Parser (Expression String)
parseVariable = fmap Variable parseIdentifier

-- parse bracketed expression
parseBrExpression :: Parser (Expression String)
parseBrExpression = parseToken LParen *> parseExpression <* parseToken RParen

parseLiteral :: Parser (Expression String)
parseLiteral = fmap Literal $ parseInt <|> parseArrayLiteral <|> parseTupleLiteral <|> parseRecordLiteral  <|> parseFunctionLiteral

parseInt :: Parser (Literal String)
parseInt = Constant <$> Parser (\x -> 
    case x of
         (AnnotatedToken (Integer z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)

parseArrayLiteral :: Parser (Literal String)
parseArrayLiteral = fmap ArrayLiteral $ parseToken LSqParen *> collect parseExpression (parseToken Comma) <* parseToken RSqParen

-- change this so (expr) is parseerror
parseTupleLiteral :: Parser (Literal String)
parseTupleLiteral = fmap TupleLiteral $ parseToken LParen *> collect parseExpression (parseToken Comma) <* parseToken RParen

parseRecordLiteral :: Parser (Literal String)
parseRecordLiteral = fmap RecordLiteral $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Equals *> parseExpression)) (parseToken Comma) <* parseToken RCrParen

-- \a -> {}
parseFunctionLiteral :: Parser (Literal String)
parseFunctionLiteral = liftA2 FunctionLiteral (parseToken BSlash *> parseIdentifier <* parseToken Arrow) (parseExpression)

parseBody :: Parser (Body String)
parseBody = fmap Body (collectM parseStatement $ parseToken Term)

parseStatement :: Parser (Statement String)
parseStatement = fmap Defn parseDefinition <|> parseAssignment <|> fmap Expr parseExpression

parseAssignment :: Parser (Statement String)
parseAssignment = liftA2 Assignment (parseIdentifier <* parseToken Equals) parseExpression
-- UnionLiteral ((String, Expression), [(String, Type)])
-- remove union literals for now. not completely sure on syntax for them yet
-- parseUnionLiteral = fmap UnionLiteral $ parseToken LCrParen *> liftA2 (\x y -> (x, y)) () () <* parseToken RCrParen

parseFunctionCall :: (Expression String) -> Parser (Expression String)
parseFunctionCall t = liftA2 FunctionCall (pure t) parseExpression
