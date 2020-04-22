module Parser (parseFile, run) where

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
parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parseIdentifier ((parseToken Colon *> fmap Just parseType) <|> (parseToken Colon $> Nothing)) (parseToken Equals *> parseExpressionA)


-- expressionAll. here infx is allowed. typically it isn't, so parseExpression will not parse infx
parseExpressionA :: Parser (Expression String)
parseExpressionA = fmap orderOps parseInfixOp

parseExpression :: Parser (Expression String)
parseExpression = infbuild (parseBrExpression <|> parseBlock <|> parseIfStmt <|> parseLiteral <|> parseVariable) parseFunctionCall

parseIfStmt :: Parser (Expression String)
parseIfStmt = liftA3 IfStmt (parseToken If *> parseExpressionA) (parseToken Then *> parseExpressionA) (parseToken Else *> parseExpressionA)

parseVariable :: Parser (Expression String)
parseVariable = fmap Variable parseIdentifier

-- parse bracketed expression
parseBrExpression :: Parser (Expression String)
parseBrExpression = parseToken LParen *> parseExpressionA <* parseToken RParen

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

parseBlock :: Parser (Expression String)
parseBlock = fmap Block (parseToken LCrParen *> (collectM parseStatement $ parseToken Term) <* parseToken RCrParen)

parseStatement :: Parser (Statement String)
parseStatement = fmap Defn parseDefinition <|> parseAssignment <|> fmap Expr parseExpression

parseAssignment :: Parser (Statement String)
parseAssignment = liftA2 Assignment (parseIdentifier <* parseToken Equals) parseExpression
-- UnionLiteral ((String, Expression), [(String, Type)])
-- remove union literals for now. not completely sure on syntax for them yet
-- parseUnionLiteral = fmap UnionLiteral $ parseToken LCrParen *> liftA2 (\x y -> (x, y)) () () <* parseToken RCrParen

parseFunctionCall :: (Expression String) -> Parser (Expression String)
parseFunctionCall t = liftA2 FunctionCall (pure t) parseExpression

-- an infix operation. int = priority, token = lexical token (for example, +)
data Operation = Operation Int Token String

instance Disp Operation where
    disp (Operation nt t s) = (show t)

data InfixExpr a = InfixExpr (Expression a) (OpExpr a)
instance (Disp a) => Disp (InfixExpr a) where
    disp (InfixExpr e o) = (disp e) ++ (disp o)

data OpExpr a = OpExpr Operation (InfixExpr a) | Null
instance (Disp a) => Disp (OpExpr a) where
    disp (OpExpr o a) = (disp o) ++ (disp a)
    disp Null = ""
    


parseInfixOp :: Parser (InfixExpr String)
parseInfixOp = Parser (\ts -> 
    case run parseExpression ts of
         ParseFailure -> ParseFailure
         Unrecoverable r -> Unrecoverable r
         (ParseSuccess expr ts2) -> case run parseOp ts2 of
                                         ParseFailure -> ParseSuccess (InfixExpr expr Null) ts2
                                         Unrecoverable r -> Unrecoverable r
                                         ParseSuccess op ts3 -> run (fmap (\x -> (InfixExpr expr (OpExpr op x))) parseInfixOp) ts3)
                                         {- case run parseInfixOp ts3 of
                                                                     ParseFailure -> ParseFailure
                                                                     Unrecoverable r -> Unrecoverable r
                                                                     ParseSuccess iexpr ts4 -> ParseSuccess (InfixExpr expr (OpExpr op iexpr)) ts4) -}

parseOp = (parseToken Plus $> Operation 1 Plus "+") <|>
          (parseToken Mult $> Operation 2 Mult "*") <|> 
          (parseToken FSlash $> Operation 2 FSlash "/") <|> 
          (parseToken Minus $> Operation 2 Minus "-")

orderOps iexpr = case (lower 1 (lower 2 iexpr)) of
                      (InfixExpr e Null) -> e
                      _ -> error "Operator ordering failure, see Parser.hs. #259374878345789354789" -- number to easily find where the code is 

-- "lowers" an op to an expr.
lower prior (InfixExpr e (OpExpr (Operation n t st) (InfixExpr e2 rest)))
    | prior == n = lower prior (InfixExpr (FunctionCall (Variable st) (Literal (TupleLiteral [e, e2]))) rest)
    | otherwise = (InfixExpr e (OpExpr (Operation n t st) (lower prior (InfixExpr e2 rest))))

lower prior p = p
