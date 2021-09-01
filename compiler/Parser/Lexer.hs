{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Parser.Lexer (AnnotatedToken(..), Token(..), lexFile, passLexer) where 

import Common.Common
import Common.Pass
import Common.Reporting

import Data.List
import Data.Maybe
import Data.Char
import Data.Bifunctor
import Control.Applicative
import Control.DeepSeq
import GHC.Generics

data Token = LParen | RParen | LSqParen | RSqParen | LCrParen | RCrParen | Integer Int | Character Char | String String | Identifier String
           | Term | Comma | Equals | Return | Yield | PlusEquals | Pipe | New | Void | Type | Colon | Dot | Arrow | If | Then
           | Plus | DoubleEquals | Less | Greater | Minus | Mult | Ampersand | Caret | Tru | Fals | FSlash | BSlash
           | Exclamation | Else | GreaterEqual | LesserEqual | Import | Export | Error | Forall | Dollar | DoublePlus deriving (Show, Eq)

deriving instance Generic Token
deriving instance NFData Token

{- an token annotated with other data such as line number, characters held -}
data AnnotatedToken = AnnotatedToken{
    annLexeme :: Token,
    annLineStart :: Int,
    annLineEnd :: Int,
    annSrcString :: String,
    annColStart :: Int,
    annColEnd :: Int
    } deriving (Show, Eq)

deriving instance Generic AnnotatedToken
deriving instance NFData AnnotatedToken
    
instance Disp AnnotatedToken where
    -- convert \n to ; to not mess up output
    disp ann = case annLexeme ann of
                    Term -> "; line: " ++ show (annLineStart ann) ++ " " ++ show (annLexeme ann)
                    otherwise -> (annSrcString ann) ++ " line: " ++ show (annLineStart ann) ++ " " ++ show (annLexeme ann)
    
instance Disp ([] AnnotatedToken) where
    disp r = intercalate "\n" (map disp r)

passLexer = Pass {pName = "Lexing", pFunc = doLx}
    where doLx s = let result = lexFile s in case filterE result of
                                                  [] -> (mempty, Just (result))
                                                  es -> (foldr (<>) mempty (map (e2Msg s) es), Nothing)

e2Msg s ann = createReportMsg $ (
    Report {
        msgMainText = "Encountered Invalid Symbol",
        msgExcerpt = s,
        msgFileName = Nothing,
        msgSeverity = Common.Reporting.Error,
        msgErrorCode = 1,
        msgPassName = "Lexer",
        msgAnnotations = [((annLineStart ann, annColStart ann, annLineEnd ann, annColEnd ann), "Here")],
        msgNote = ["This character or character sequence is not a valid lexical symbol"]
    })

filterE = filter (\x -> annLexeme x == Parser.Lexer.Error)
                       
lexFile :: String -> [AnnotatedToken]
lexFile str = lexLine str 1 1

lexLine :: String -> Int -> Int -> [AnnotatedToken]
-- eof
lexLine "" ln col = []
-- ignore whitespace
lexLine (' ':tr) ln col = lexLine tr ln (col+1)
lexLine ('/':'/':tr) ln col = lexLine (nextLine tr) (ln+1) 1
lexLine ('/':'*': tr) ln col = skipToEnd tr ln (col+2)
    where 
    skipToEnd ('*': '/' : tr) ln col = lexLine tr ln (col+2)
    skipToEnd ('\n':tr) ln col = skipToEnd tr (ln+1) 1
    skipToEnd (a:tr) ln col = skipToEnd tr ln (col+1)
    skipToEnd [] ln col = []

-- compare input and output from lexone, update metadata
lexLine str ln col = anntok : (lexLine rest newln newcol)
    where (token, rest) = lexOne str
          missing = take (length str - length rest) str
          newln = if '\n' `elem` missing then ln + 1 else ln
          newcol = if '\n' `elem` missing then 1 else col + (length missing)
          anntok = AnnotatedToken {
              annLexeme = token,
              annSrcString = missing,
              annLineStart = ln,
              annLineEnd = if token == Parser.Lexer.Error then ln else newln,
              annColStart = col,
              annColEnd = if token == Parser.Lexer.Error then col else (newcol-1)
          }

nextLine :: String -> String
nextLine ('\n':tr) = tr
nextLine (s:tr) = nextLine tr
nextLine _ = ""

-- note: partial function is safe here due to Just at end.
-- also: left must take priority over right.
lexOne :: String -> (Token, String)
lexOne str = fromJust (lexSym str <|> lexNum str <|> lexChar str <|> lexKw str <|> lexIdent str <|> lexStr str <|> Just (Parser.Lexer.Error, ""))

{-
Lexing functions are string -> maybe (token, string)
where function produces a token and remaining input if successful and nothing otherwise
-}
    
-- symbol
lexSym :: String -> Maybe (Token, String)
lexSym ('=' : '=' : str) = Just (DoubleEquals, str)
lexSym ('>' : '=' : str) = Just (GreaterEqual, str)
lexSym ('<' : '=' : str) = Just (LesserEqual, str)
lexSym ('+' : '=' : str) = Just (PlusEquals, str)
lexSym ('-' : '>' : str) = Just (Arrow, str)
lexSym ('+' : '+' : str) = Just (DoublePlus, str)
lexSym ('(' : str) = Just (LParen, str)
lexSym (')' : str) = Just (RParen, str)
lexSym ('[' : str) = Just (LSqParen, str)
lexSym (']' : str) = Just (RSqParen, str)
lexSym ('{' : str) = Just (LCrParen, str)
lexSym ('}' : str) = Just (RCrParen, str)
lexSym (',' : str) = Just (Comma, str)
lexSym ('.' : str) = Just (Dot, str)
lexSym ('=' : str) = Just (Equals, str)
lexSym ('+' : str) = Just (Plus, str)
lexSym ('*' : str) = Just (Mult, str)
lexSym ('<' : str) = Just (Less, str)
lexSym ('>' : str) = Just (Greater, str)
lexSym ('-' : str) = Just (Minus, str)
lexSym ('&' : str) = Just (Ampersand, str)
lexSym ('^' : str) = Just (Caret, str)
lexSym ('|' : str) = Just (Pipe, str)
lexSym ('!' : str) = Just (Exclamation, str)
lexSym (':' : str) = Just (Colon, str)
lexSym (';' : str) = Just (Term, str)
lexSym ('\n' : str) = Just (Term, str)
lexSym ('\\' : str) = Just (BSlash, str)
lexSym ('/' : str) = Just (FSlash, str)
lexSym ('$' : str) = Just (Dollar, str)
lexSym ('∀' : str) = Just (Forall, str)
lexSym _ = Nothing

lexIdent :: String -> Maybe (Token, String)
lexIdent str = first Identifier <$> extractIdent str

extractIdent :: String -> Maybe (String, String)
extractIdent (s:tr) = if isAlpha s || s == '_' then Just (s:u, rest) else Nothing
    where (u, rest) = extractIdent2 tr
extractIdent _ = Nothing

extractIdent2 :: String -> (String, String)
extractIdent2 (s:tr) = if isAlphaNum s || s == '_' then (s:u, rest) else ("", s:tr)
    where (u, rest) = extractIdent2 tr
extractIdent2 str = ("", str)

lexNum :: String -> Maybe (Token, String)
-- minus case handled as unary (-) operator
lexNum str = if num == "" then Nothing else Just (Integer (read num::Int), rest)
    where (num, rest) = extractNumAsStr str 

extractNumAsStr :: String -> (String, String)
extractNumAsStr (s:tr) = if isNumber s then (s: t, rest) else ("", s:tr)
    where (t, rest) = extractNumAsStr tr
extractNumAsStr str = ("", str)


-- keyword
lexKw :: String -> Maybe (Token, String)
lexKw str
    | kwMatch str "if" = Just (If, drop 2 str)
    | kwMatch str "then" = Just (Then, drop 4 str)
    | kwMatch str "else" = Just (Else, drop 4 str)
    | kwMatch str "true" = Just (Tru, drop 4 str)
    | kwMatch str "false" = Just (Fals, drop 5 str)
    | kwMatch str "type" = Just (Type, drop 4 str)
    | kwMatch str "return" = Just (Return, drop 6 str)
    | kwMatch str "yield" = Just (Yield, drop 5 str)
    | kwMatch str "import" = Just (Import, drop 6 str)
    | kwMatch str "export" = Just (Export, drop 6 str)
    | kwMatch str "forall" = Just (Forall, drop 6 str)
    | otherwise = Nothing
 
kwMatch :: String -> String -> Bool
kwMatch (s:tr) (k:w) = s == k && kwMatch tr w
-- matched kw, but does input continue?
kwMatch (s:tr) _ = not (isAlphaNum s)
-- end of input, keyword not complete
kwMatch _ (k:w) = False
kwMatch _ _ = False

lexChar :: String -> Maybe (Token, String)
-- '' = error
lexChar ('\'' : '\'' : rest) = Nothing
-- '\b'
lexChar ('\'' : '\\' : b : '\'' : rest) = (\x -> (Character x, rest)) <$> escapeC b
-- 'a'
lexChar ('\'' : a : '\'': rest) = Just (Character a, rest)
lexChar _ = Nothing

escapeC :: Char -> Maybe Char
escapeC 'n' = Just '\n'
escapeC '\\' = Just '\\'
escapeC '\'' = Just '\''
escapeC _ = Nothing

lexStr :: String -> Maybe (Token, String)
lexStr ('"':rest) = (\x -> (String x, drop num rest)) <$> str
    where (str, num) = getStr rest
lexStr _ = Nothing


-- string
getStr :: Num b => String -> (Maybe String, b)
getStr (a:b:r)
    | a == '\\' = (liftA2 (:) (escape b) x0, r0 + 2)
    | a == '"' = (Just "", 1)
    | otherwise =  (liftA2 (:) (Just a)  x1, r1 + 1)
    where (x0, r0) = getStr r
          (x1, r1) = getStr (b:r)
-- endqoute is also end of input
getStr "\"" = (Just "", 1)
-- no ending quote, error
getStr _ = (Nothing, 0)

escape :: Char -> Maybe Char
escape 'n' = Just '\n'
escape '\\' = Just '\\'
escape '"' = Just '"'
escape _ = Nothing
