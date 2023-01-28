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

parseUnderscore :: Parser ()
parseUnderscore = Parser (\x ->
    case x of
         ((AnnotatedToken{annLexeme=(Identifier "_")}):zs) -> ParseSuccess () zs
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
parseTuple = fmap Tuple $ ((parseToken LParen *> collect parseMonoType (parseToken Comma) <* parseToken RParen) <|> (parseToken LParen *> pure [] <* parseToken RParen))

parseRecord :: Parser MonoType
parseRecord = fmap Record $ parseToken LCrAndParen *> collect (liftA2 (,) parseIdentifier (parseToken Colon *> parseMonoType)) (parseToken Comma) <* parseToken RCrAndParen

parseUnion :: Parser MonoType
parseUnion = fmap Union $ parseToken LCrOrParen *> collect (liftA2 (,) parseIdentifier (parseToken Colon *> parseMonoType)) (parseToken Comma)  <* parseToken RCrOrParen

parseDefinition :: Parser (Definition String)
parseDefinition = liftA3 (\x y z -> Definition {identifier=x, typeof=y, value=z}) parsePatMatch ((parseToken Colon *> fmap Just parseType) <|> (parseToken Colon $> Nothing)) (parseToken Equals *> expect "Expected Expression in Definition" parseAnnExprA)

parseAnnExpr :: Parser (AnnExpr String)
parseAnnExpr = liftA2 (\x y -> AnnExpr {aExpr = x, aId = 0, aType = y}) parseExpression ((parseToken Colon *> fmap Just parseType) <|> pure Nothing)

parseAnnExprA :: Parser (AnnExpr String)
parseAnnExprA = liftA2 (\x y -> AnnExpr {aExpr = x, aId = 0, aType = y}) parseExpressionA ((parseToken Colon *> fmap Just parseType) <|> pure Nothing)

-- expressionAll. here infx is allowed. typically it isn't, so parseExpression will not parse infx
parseExpressionA :: Parser (Expression String)
parseExpressionA = fmap orderOps parseInfixOp

parseExpression :: Parser (Expression String)
parseExpression = infbuild (parseInitalization <|> parseMatching <|> (infbuild parseBrExpression parseSelector) <|> parseBlock <|> parseIfStmt <|> parseLiteral <|> (infbuild parseVariable parseSelector)) parseFunctionCall

parseIfStmt :: Parser (Expression String)
parseIfStmt = liftA3 IfStmt (parseToken If *> parseAnnExprA) (parseToken Then *> parseAnnExprA) (parseToken Else *> parseAnnExprA)

parseVariable :: Parser (Expression String)
parseVariable = fmap Variable parseIdentifier

-- parse bracketed expression
parseBrExpression :: Parser (Expression String)
parseBrExpression = parseToken LParen *> parseExpressionA <* parseToken RParen

parseInitalization :: Parser (Expression String)
parseInitalization = liftA2 Initialize (parseToken New *> parseIdentifier) (parseAnnExprA)

parseMatching :: Parser (Expression String)
parseMatching = liftA2 (\x y -> PatMatching (Matching x y)) (parseToken Match *> parseAnnExprA <* parseToken With) (parseToken LCrParen *> (collectM parseMatchRow $ parseToken Term) <* parseToken RCrParen)
    where
    parseMatchRow = liftA2 (,) (parseMatchRowLHS) (parseToken EqArrow *> parseAnnExprA)
    parseMatchRowLHS = ((parseToken LCrOrParen *> liftA2 MVariant parseIdentifier parseMatchRowLHS <* parseToken RCrOrParen)) <|> (RMatch <$> (parseToken LParen *> (collectM parseMatchRowLHS $ parseToken Comma) <* parseToken RParen)) <|> (MVariable <$> parseIdentifier) <|> (parseUnderscore *> pure MNullVar)

parseLiteral :: Parser (Expression String)
parseLiteral = parseInt <|>
               parseFloat <|>
               parseBoolLiteral <|>
               parseStringLiteral <|>
               parseArrayLiteral <|>
               parseTupleLiteral <|>
               parseRecordLiteral  <|>
               parseVariantLiteral <|>
               parseFunctionLiteral

parseAssignLHS :: Parser (AssignLHS String)
parseAssignLHS = infbuild (fmap change parsePatMatch) magic2
    where change (Plain a) = Singleton a []
          change (TupleUnboxing t) = TupleUnboxingA t
          magic2 (TupleUnboxingA t) = Parser (\x -> ParseFailure)
          magic2 (Singleton a rs) = liftA2 (\x y -> Singleton a (rs ++ [(x, y)])) ((parseToken Arrow *> pure SelArrow) <|> (parseToken Dot *> pure SelDot)) parseIdentifier

parsePatMatch :: Parser (TupDestruct String)
parsePatMatch = (parseToken LParen *> (pure (TupleUnboxing [])) <* parseToken RParen) <|>
                (fmap Plain parseIdentifier) <|>
                fmap change (fmap TupleUnboxing $ parseToken LParen *> collect parseIdentifier (parseToken Comma) <* parseToken RParen)
    where change (TupleUnboxing [a]) = Plain a
          change b = b

parseInt :: Parser (Expression String)
parseInt = Constant <$> Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(Integer z)}):zs) -> ParseSuccess z zs
         _ -> ParseFailure)

parseFloat :: Parser (Expression String)
parseFloat = FloatLiteral <$> Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(Float z)}):zs) -> ParseSuccess z zs
         _ -> ParseFailure)
         
parseBoolLiteral :: Parser (Expression String)
parseBoolLiteral = fmap BooleanLiteral $ (parseToken Tru *> pure True <|> parseToken Fals *> pure False)

parseStringLiteral :: Parser (Expression String)
parseStringLiteral = StringLiteral <$> Parser (\x -> 
    case x of
         ((AnnotatedToken{annLexeme=(String text)}):zs) -> ParseSuccess text zs
         _ -> ParseFailure)

parseArrayLiteral :: Parser (Expression String)
parseArrayLiteral = fmap ArrayLiteral $ parseToken LSqParen *> collect parseAnnExprA (parseToken Comma) <* parseToken RSqParen

-- TODO: change this so (expr) is parseerror ??? might not be nessecary. 
parseTupleLiteral :: Parser (Expression String)
parseTupleLiteral = (parseToken LParen *> (pure (TupleLiteral [])) <* parseToken RParen) <|> -- edge case for "empty tuples", our unit type.
                    (fmap TupleLiteral $ parseToken LParen *> collect parseAnnExprA (parseToken Comma) <* parseToken RParen)

parseRecordLiteral :: Parser (Expression String)
parseRecordLiteral = fmap RecordLiteral $ parseToken LCrAndParen *> collect (liftA2 (,) parseIdentifier (parseToken Equals *> parseAnnExprA)) (parseToken Comma) <* parseToken RCrAndParen

parseVariantLiteral :: Parser (Expression String)
parseVariantLiteral = fmap VariantLiteral (parseToken LCrOrParen *> (liftA2 (,) parseIdentifier (parseToken Equals *> parseAnnExprA)))  <* parseToken RCrOrParen

-- \a -> {}
-- or: \(x,y,z) -> {}

-- NOTE: allowed to infixexpr here. important to use parseExpressionA here so \a -> 1+a gets interpreted as \a->(1+a) instead of (\a->1)+a
parseFunctionLiteral :: Parser (Expression String)
parseFunctionLiteral = liftA2 FunctionLiteral (parseToken BSlash *> parsePatMatch <* parseToken Arrow) (expect "Expected Expression in Function Literal" parseAnnExprA)

-- this is when (x,y,z) = n or in function?
-- parseUnTuple =

parseBlock :: Parser (Expression String)
parseBlock = fmap Block (parseToken LCrParen *> (collectM parseStatement $ parseToken Term) <* parseToken RCrParen)

parseStatement :: Parser (Statement String)
parseStatement = fmap Defn parseDefinition <|> parseReturn <|> parseYield <|> parseAssignment <|> fmap Expr parseAnnExprA

parseReturn :: Parser (Statement String)
parseReturn = fmap AST.AST.Return $ parseToken Parser.Lexer.Return *> parseAnnExprA

parseYield :: Parser (Statement String)
parseYield = fmap AST.AST.Yield $ parseToken Parser.Lexer.Yield *> parseAnnExprA

parseAssignment :: Parser (Statement String)
parseAssignment = liftA2 Assignment (parseAssignLHS <* parseToken Equals) parseAnnExprA

parseFunctionCall :: (Expression String) -> Parser (Expression String)
parseFunctionCall t = liftA2 FunctionCall (pure AnnExpr {aExpr = t, aId = 0, aType = Nothing}) parseAnnExpr

parseSelector :: (Expression String) -> Parser (Expression String)
parseSelector t = liftA3 Selector (pure AnnExpr {aExpr = t, aId = 0, aType = Nothing}) ((parseToken Arrow *> pure SelArrow) <|> (parseToken Dot *> pure SelDot)) parseIdentifier


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
    | prior == n = lower prior (InfixExpr (FunctionCall (AnnExpr {aExpr = (Variable st), aId = 0, aType = Nothing}) ((AnnExpr {aExpr = TupleLiteral [(AnnExpr {aExpr = e, aId = 0, aType = Nothing}), (AnnExpr {aExpr = e2, aId = 0, aType = Nothing})], aId = 0, aType = Nothing}))) rest)
    | otherwise = (InfixExpr e (OpExpr (Operation n t st) (lower prior (InfixExpr e2 rest))))

lower prior p = p
