{--
Parser.hs - this transformes the unstructured lexemes into the structured parse tree (AST String)
--}

module Parser.Parser (parseFile, passParse, run) where

import Common.Common
import Common.Reporting
import Parser.ParserCore
import AST.AST
import Parser.Lexer
import qualified Parser.ParserRels (parseFile)
import Common.Pass

import Control.Applicative
import Control.Category
import Data.List
import Data.Functor


passParse :: Pass [AnnotatedToken] (AST String)
passParse = Pass {pName = "Parser", pFunc = doPs}
    where doPs x = case run parseFile x of
                        ParseSuccess n t -> (mempty, Just n)
                        Unrecoverable r -> (e2msg "a" r, Nothing)
                        ParseFailure -> (messageNoLn "Parser" ((disp x) <> "Error parsing") Common.Pass.Error, Nothing)


e2msg :: String -> [Reason] -> Messages
e2msg text (r:rs) = createReportMsg(
    Report {
        msgMainText = rMessage r,
        msgExcerpt = "",
        msgFileName = Nothing,
        msgSeverity = Common.Reporting.Error,
        msgErrorCode = 2,
        msgPassName = "Parser",
        msgAnnotations = [(rSrcinfo r, "Here")],
        msgNote = []
    }) <> (e2msg text rs)

e2msg text [] = mempty
    
parseFile :: Parser (AST String)
parseFile = Parser.ParserRels.parseFile *> ((liftA2 (\x y -> AST{astTypes = x, astDefns = y}) (collectM parseTypeDefn (parseToken Term) <|> ((collectM (pure ()) $ parseToken Term) *> pure []))
                                (collectM parseDefinition (parseToken Term))) <|>
                                ((collectM (pure ()) $ parseToken Term) *>
                                parseEOF *> pure (AST {astDefns = [], astTypes = []})))

-- parses an identifier
parseIdentifier :: Parser String
parseIdentifier = Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(Identifier z)}):zs) -> ParseSuccess z zs
         _ -> ParseFailure)         

parseMonoType :: Parser MonoType
parseMonoType = parseBType <|> infbuild (parseRecord <|> parseUnion <|> parseIntType <|> parseBoolType <|> parseStringType <|> parseArrayType <|> parseTuple <|> parseTV <|> parseDType) parseFunctionType

parseType :: Parser Type
parseType = liftA2 Poly parseQuant parseMonoType <|> fmap (Poly []) parseMonoType -- <|> todo this stuff parseToken (Forall) *> collect ()(parseToken Comma)

parseQuant :: Parser [Int]
parseQuant = parseToken Forall *> collect pint (parseToken Comma) <* parseToken Dot
    where pint = Parser (\x -> case x of
                    ((AnnotatedToken{annLexeme=(Integer z)}):zs) -> ParseSuccess z zs
                    _ -> ParseFailure)

-- bracketed type
parseBType :: Parser MonoType
parseBType = parseToken LParen *> parseMonoType <* parseToken RParen

parseTV :: Parser MonoType
parseTV = parseToken Dollar *> (General <$> Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(Integer z)}):zs) -> ParseSuccess z zs
         _ -> ParseFailure))

parseIntType :: Parser MonoType
parseIntType = parseToken (Identifier "Int") $> IntT

parseBoolType :: Parser MonoType
parseBoolType = parseToken (Identifier "Boolean") $> BoolT

parseStringType :: Parser MonoType
parseStringType = parseToken (Identifier "String") $> StringT

parseArrayType :: Parser MonoType
parseArrayType = parseToken LSqParen *> fmap Array parseMonoType <* parseToken RSqParen

parseFunctionType :: MonoType -> Parser MonoType
parseFunctionType t = liftA2 Function (pure t) (parseToken Arrow *> parseMonoType)

parseDType :: Parser MonoType
parseDType = liftA2 DType parseIdentifier parseMonoType -- ?FIXME?

-- parsing of <Int, Bool> for example
parseTriangleTy :: Parser [MonoType]
parseTriangleTy = (parseToken Less *> collect parseMonoType (parseToken Comma) <* parseToken Greater) <|> pure []

parseTuple :: Parser MonoType
parseTuple = fmap Tuple $ parseToken LParen *> collect parseMonoType (parseToken Comma) <* parseToken RParen

parseRecord :: Parser MonoType
parseRecord = fmap Record $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseMonoType)) (parseToken Comma) <* parseToken RCrParen

-- future optimization: roll parseRecord and parseUnion into one func
parseUnion :: Parser MonoType
parseUnion = fmap Union $ parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Colon *> parseMonoType)) (parseToken Pipe) <* parseToken RCrParen

parseDefinition :: Parser (Definition String)
parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parsePatMatch ((parseToken Colon *> fmap Just parseType) <|> (parseToken Colon $> Nothing)) (parseToken Equals *> expect "Expected Expression in Definition" parseExpressionA)


-- expressionAll. here infx is allowed. typically it isn't, so parseExpression will not parse infx
parseExpressionA :: Parser (Expression String)
parseExpressionA = fmap orderOps parseInfixOp

parseExpression :: Parser (Expression String)
parseExpression = infbuild (parseInitalization <|> (infbuild parseBrExpression parseSelector) <|> parseBlock <|> parseIfStmt <|> parseLiteral <|> (infbuild parseVariable parseSelector)) parseFunctionCall

parseIfStmt :: Parser (Expression String)
parseIfStmt = liftA3 IfStmt (parseToken If *> parseExpressionA) (parseToken Then *> parseExpressionA) (parseToken Else *> parseExpressionA)

parseVariable :: Parser (Expression String)
parseVariable = fmap Variable parseIdentifier

-- parse bracketed expression
parseBrExpression :: Parser (Expression String)
parseBrExpression = parseToken LParen *> parseExpressionA <* parseToken RParen

parseInitalization :: Parser (Expression String)
parseInitalization = liftA2 Initialize (parseToken New *> parseIdentifier) (fmap (\(Literal l) -> l) parseLiteral)

parseLiteral :: Parser (Expression String)
parseLiteral = fmap Literal $ parseInt <|>
                              parseBoolLiteral <|>
                              parseStringLiteral <|>
                              parseArrayLiteral <|>
                              parseTupleLiteral <|>
                              parseRecordLiteral  <|>
                              parseFunctionLiteral

parseAssignLHS :: Parser (AssignLHS String)
parseAssignLHS = infbuild (fmap change parsePatMatch) magic2
    where change (Plain a) = Singleton a []
          change (TupleUnboxing t) = TupleUnboxingA t
          magic2 (TupleUnboxingA t) = pure $ TupleUnboxingA t
          magic2 (Singleton a rs) = liftA2 (\x y -> Singleton a (rs ++ [(x, y)])) ((parseToken Arrow *> pure SelArrow) <|> (parseToken Dot *> pure SelDot)) parseIdentifier

parsePatMatch :: Parser (PatternMatching String)
parsePatMatch = (parseToken LParen *> (pure (TupleUnboxing [])) <* parseToken RParen) <|>
                (fmap Plain parseIdentifier) <|>
                fmap change (fmap TupleUnboxing $ parseToken LParen *> collect parseIdentifier (parseToken Comma) <* parseToken RParen)
    where change (TupleUnboxing [a]) = Plain a
          change b = b

parseInt :: Parser (Literal String)
parseInt = Constant <$> Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(Integer z)}):zs) -> ParseSuccess z zs
         _ -> ParseFailure)
         
parseBoolLiteral :: Parser (Literal String)
parseBoolLiteral = fmap BooleanLiteral $ (parseToken Tru *> pure True <|> parseToken Fals *> pure False)

parseStringLiteral :: Parser (Literal String)
parseStringLiteral = StringLiteral <$> Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(String text)}):zs) -> ParseSuccess text zs
         _ -> ParseFailure)

parseArrayLiteral :: Parser (Literal String)
parseArrayLiteral = fmap ArrayLiteral $ parseToken LSqParen *> collect parseExpressionA (parseToken Comma) <* parseToken RSqParen

-- TODO: change this so (expr) is parseerror ??? might not be nessecary. 
parseTupleLiteral :: Parser (Literal String)
parseTupleLiteral = (parseToken LParen *> (pure (TupleLiteral [])) <* parseToken RParen) <|> -- edge case for "empty tuples", our unit type.
                    (fmap TupleLiteral $ parseToken LParen *> collect parseExpressionA (parseToken Comma) <* parseToken RParen)

parseRecordLiteral :: Parser (Literal String)
parseRecordLiteral = fmap RecordLiteral $ parseToken At *> parseToken LCrParen *> collect (liftA2 (\x y -> (x, y)) parseIdentifier (parseToken Equals *> parseExpressionA)) (parseToken Comma) <* parseToken RCrParen

-- \a -> {}
-- or: \(x,y,z) -> {}

-- NOTE: allowed to infixexpr here. important to use parseExpressionA here so \a -> 1+a gets interpreted as \a->(1+a) instead of (\a->1)+a
parseFunctionLiteral :: Parser (Literal String)
parseFunctionLiteral = liftA2 FunctionLiteral (parseToken BSlash *> parsePatMatch <* parseToken Arrow) (expect "Expected Expression in Function Literal" parseExpressionA)

-- this is when (x,y,z) = n or in function?
-- parseUnTuple =

parseBlock :: Parser (Expression String)
parseBlock = fmap Block (parseToken LCrParen *> (collectM parseStatement $ parseToken Term) <* parseToken RCrParen)

parseStatement :: Parser (Statement String)
parseStatement = fmap Defn parseDefinition <|> parseReturn <|> parseYield <|> parseAssignment <|> fmap Expr parseExpressionA

parseReturn :: Parser (Statement String)
parseReturn = fmap AST.AST.Return $ parseToken Parser.Lexer.Return *> parseExpressionA

parseYield :: Parser (Statement String)
parseYield = fmap AST.AST.Yield $ parseToken Parser.Lexer.Yield *> parseExpressionA

parseAssignment :: Parser (Statement String)
parseAssignment = liftA2 Assignment (parseAssignLHS <* parseToken Equals) parseExpressionA
-- UnionLiteral ((String, Expression), [(String, Type)])
-- remove union literals for now. not completely sure on syntax for them yet
-- parseUnionLiteral = fmap UnionLiteral $ parseToken LCrParen *> liftA2 (\x y -> (x, y)) () () <* parseToken RCrParen

parseFunctionCall :: (Expression String) -> Parser (Expression String)
parseFunctionCall t = liftA2 FunctionCall (pure t) parseExpression

parseSelector :: (Expression String) -> Parser (Expression String)
parseSelector t = liftA3 Selector (pure t) ((parseToken Arrow *> pure SelArrow) <|> (parseToken Dot *> pure SelDot)) parseIdentifier


parseTypeDefn :: Parser DefinedType
parseTypeDefn = liftA3 (\x y z -> DefinedType{dtName = x, dtBindings = y, dtType = z}) (parseToken Type *> parseIdentifier) (parseTriangleTy2 <* parseToken Equals) parseMonoType

-- like parseTriangleTy but only allows $n i.e general bindings.
parseTriangleTy2 :: Parser [Int]
parseTriangleTy2 = (parseToken Less *> collect helper (parseToken Comma) <* parseToken Greater) <|> pure []
    where helper = parseToken Dollar *> Parser (\x -> 
                    case x of
                        ((AnnotatedToken{annLexeme=(Integer z)}):zs) -> ParseSuccess z zs
                        _ -> ParseFailure)

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
          (parseToken LesserEqual $> Operation 3 LesserEqual "<=") <|>
          (parseToken DoublePlus $> Operation 4 DoublePlus "++")

orderOps iexpr = case (lower 0 (lower 1 (lower 2 (lower 3 (lower 4 (lower 5 iexpr)))))) of
                      (InfixExpr e Null) -> e
                      _ -> error "Operator ordering failure, see Parser.hs. #259374878345789354789" -- number to easily find where the code is 

-- "lowers" an op to an expr.
lower prior (InfixExpr e (OpExpr (Operation n t st) (InfixExpr e2 rest)))
    | prior == n = lower prior (InfixExpr (FunctionCall (Variable st) (Literal (TupleLiteral [e, e2]))) rest)
    | otherwise = (InfixExpr e (OpExpr (Operation n t st) (lower prior (InfixExpr e2 rest))))

lower prior p = p
