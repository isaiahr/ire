module Parser.Parser (parseFile, passParse, run) where

import Common.Common
import Parser.ParserCore
import AST.AST
import Parser.Lexer
import Common.Pass

import Control.Applicative
import Control.Category
import Data.List
import Data.Functor


-- runs parser on tokenstream
run :: Parser p -> [AnnotatedToken] -> ParseResult p
run (Parser ps) = ps

passParse :: Pass [AnnotatedToken] (AST String)
passParse = Pass {pName = ["Parser"], pFunc = doPs}
    where doPs x = case run parseFile x of
                        ParseSuccess n t -> (messageNoLn "Parser" (disp n) Debug, Just n)
                        otherwise -> (messageNoLn "Parser" "Error parsing" Common.Pass.Error, Nothing)


parseFile :: Parser (AST String)
parseFile = fmap AST $ collectM parseDefinition $ parseToken Term

-- parses an identifier
parseIdentifier :: Parser String
parseIdentifier = Parser (\x -> 
    case x of
         (AnnotatedToken (Identifier z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)         

parseType :: Parser Type
parseType = parseBType <|> infbuild (parseRecord <|> parseUnion <|> parseIntType <|> parseBoolType <|> parseStringType <|> parseArrayType <|> parseTuple) parseFunctionType

-- bracketed type
parseBType :: Parser Type
parseBType = parseToken LParen *> parseType <* parseToken RParen

parseIntType :: Parser Type
parseIntType = parseToken (Identifier "Int") $> Bits 64

parseBoolType :: Parser Type
parseBoolType = parseToken (Identifier "Boolean") $> Bits 1

parseStringType :: Parser Type
parseStringType = parseToken (Identifier "String") $> StringT

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
parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parsePatMatch ((parseToken Colon *> fmap Just parseType) <|> (parseToken Colon $> Nothing)) (parseToken Equals *> parseExpressionA)


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
parseLiteral = fmap Literal $ parseInt <|> parseStringLiteral <|> parseArrayLiteral <|> parseTupleLiteral <|> parseRecordLiteral  <|> parseFunctionLiteral

parsePatMatch :: Parser (PatternMatching String)
parsePatMatch = (parseToken LParen *> (pure (TupleUnboxing [])) <* parseToken RParen) <|>
                (fmap Plain parseIdentifier) <|>
                (fmap TupleUnboxing $ parseToken LParen *> collect parseIdentifier (parseToken Comma) <* parseToken RParen)

parseInt :: Parser (Literal String)
parseInt = Constant <$> Parser (\x -> 
    case x of
         (AnnotatedToken (Integer z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)

parseStringLiteral :: Parser (Literal String)
parseStringLiteral = StringLiteral <$> Parser (\x -> 
    case x of
         (AnnotatedToken (String text) l str):zs -> ParseSuccess text zs
         _ -> ParseFailure)

parseArrayLiteral :: Parser (Literal String)
parseArrayLiteral = fmap ArrayLiteral $ parseToken LSqParen *> collect parseExpression (parseToken Comma) <* parseToken RSqParen

-- TODO: change this so (expr) is parseerror ??? might not be nessecary. 
parseTupleLiteral :: Parser (Literal String)
parseTupleLiteral = (parseToken LParen *> (pure (TupleLiteral [])) <* parseToken RParen) <|> -- edge case for "empty tuples", our unit type.
                    (fmap TupleLiteral $ parseToken LParen *> collect parseExpression (parseToken Comma) <* parseToken RParen)

parseRecordLiteral :: Parser (Literal String)
parseRecordLiteral = fmap RecordLiteral $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Equals *> parseExpression)) (parseToken Comma) <* parseToken RCrParen

-- \a -> {}
-- or: \(x,y,z) -> {}
parseFunctionLiteral :: Parser (Literal String)
parseFunctionLiteral = liftA2 FunctionLiteral (parseToken BSlash *> parsePatMatch <* parseToken Arrow) (parseExpression)

-- this is when (x,y,z) = n or in function?
-- parseUnTuple =

parseBlock :: Parser (Expression String)
parseBlock = fmap Block (parseToken LCrParen *> (collectM parseStatement $ parseToken Term) <* parseToken RCrParen)

parseStatement :: Parser (Statement String)
parseStatement = fmap Defn parseDefinition <|> parseReturn <|> parseYield <|> parseAssignment <|> fmap Expr parseExpression

parseReturn :: Parser (Statement String)
parseReturn = fmap AST.AST.Return $ parseToken Parser.Lexer.Return *> parseExpressionA

parseYield :: Parser (Statement String)
parseYield = fmap AST.AST.Yield $ parseToken Parser.Lexer.Yield *> parseExpressionA

parseAssignment :: Parser (Statement String)
parseAssignment = liftA2 Assignment (parsePatMatch <* parseToken Equals) parseExpression
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
{-
higher number: binds more strongly.
if * > +, then (a+b*c) => (a+(b*c))
ties mean left to right associativity.
(so a+b-c => (a+b)-c)
also, see: https://introcs.cs.princeton.edu/java/11precedence/ for refence on (supposedly) reasonable
defaults
-}

parseOp = (parseToken Plus $> Operation 4 Plus "+") <|>
          (parseToken Minus $> Operation 4 Minus "-") <|> 
          (parseToken Mult $> Operation 5 Mult "*") <|> 
          (parseToken FSlash $> Operation 5 FSlash "/") <|> 
          (parseToken Pipe $> Operation 0 Pipe "|") <|> 
          (parseToken Ampersand $> Operation 1 Ampersand "&") <|> 
          (parseToken DoubleEquals $> Operation 2 DoubleEquals "==") <|> 
          (parseToken Greater $> Operation 3 Greater ">") <|>
          (parseToken Less $> Operation 3 Less "<") <|>
          (parseToken GreaterEqual $> Operation 3 GreaterEqual ">=") <|>
          (parseToken LesserEqual $> Operation 3 LesserEqual "<=") 

orderOps iexpr = case (lower 0 (lower 1 (lower 2 (lower 3 (lower 4 (lower 5 iexpr)))))) of
                      (InfixExpr e Null) -> e
                      _ -> error "Operator ordering failure, see Parser.hs. #259374878345789354789" -- number to easily find where the code is 

-- "lowers" an op to an expr.
lower prior (InfixExpr e (OpExpr (Operation n t st) (InfixExpr e2 rest)))
    | prior == n = lower prior (InfixExpr (FunctionCall (Variable st) (Literal (TupleLiteral [e, e2]))) rest)
    | otherwise = (InfixExpr e (OpExpr (Operation n t st) (lower prior (InfixExpr e2 rest))))

lower prior p = p
