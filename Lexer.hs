module Lexer (Token(..), lexFile) where 

import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative

data Token = LParen | RParen | LSqParen | RSqParen | LCrParen | RCrParen | Integer Int | Character Char | String [Char] | Identifier [Char]
           | Term | Comma | Equals | Return | PlusEquals | Pipe | New | Void | Type | Colon | Dot | If
           | Plus | DoubleEquals | Less | Greater | Minus | Mult | Ampersand | Caret | Tru | Fals
           | Exclamation | Else | Error deriving (Show, Eq)

{- an token annotated with other data such as line number, characters held -}
data AnnotatedToken = AnnotatedToken Token Int [Char] deriving Eq

instance Show AnnotatedToken where
    -- convert \n to ; to not mess up output
    show (AnnotatedToken token@Term ln str) = "; line: " ++ show ln ++ " " ++ show token
    show (AnnotatedToken token ln str) = str ++ " line: " ++ show ln ++ " " ++ show token

lexFile str = lexLine str 1

-- eof
lexLine "" ln = []
-- ignore whitespace
lexLine (' ':tr) ln = lexLine tr ln
lexLine ('/':'/':tr) ln = lexLine (nextLine tr) (ln+1)

-- compare input and output from lexone, update metadata
lexLine str ln = (AnnotatedToken token ln missing) : (lexLine rest newln)
    where (token, rest) = lexOne str
          missing = take (length str - length rest) str
          newln = if elem '\n' missing then ln + 1 else ln

nextLine ('\n':tr) = tr
nextLine (s:tr) = nextLine tr
nextLine _ = ""

-- note: partial function is safe here due to Just at end.
-- also: left must take priority over right.
lexOne str = fromJust (lexSym str <|> lexNum str <|> lexChar str <|> lexKw str <|> lexIdent str <|> lexStr str <|> Just (Error, ""))

{-
Lexing functions are string -> maybe (token, string)
where function produces a token and remaining input if successful and nothing otherwise
-}
    
-- symbol
lexSym ('=' : '=' : str) = Just (DoubleEquals, str)
lexSym ('+' : '=' : str) = Just (PlusEquals, str)
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
lexSym (';' : str) = Just (Term, str)
lexSym ('\n' : str) = Just (Term, str)
lexSym _ = Nothing


lexIdent str = (\(x, y) -> (Identifier x, y)) <$> extractIdent str

extractIdent (s:tr) = if isAlpha s then Just (s:u, rest) else Nothing
    where (u, rest) = extractIdent2 tr
extractIdent _ = Nothing

extractIdent2 (s:tr) = if isAlphaNum s then (s:u, rest) else ("", s:tr)
    where (u, rest) = extractIdent2 tr
extractIdent2 str = ("", str)

-- minus case handled as unary (-) operator
lexNum str = if num == "" then Nothing else Just (Integer (read num::Int), rest)
    where (num, rest) = extractNumAsStr str 

extractNumAsStr (s:tr) = if (isNumber s) then ((s: t), rest) else ("", s:tr)
    where (t, rest) = extractNumAsStr $ tr
extractNumAsStr str = ("", str)


-- keyword
lexKw str
    | kwMatch str "if" = Just (If, drop 2 str)
    | kwMatch str "else" = Just (Else, drop 4 str)
    | kwMatch str "true" = Just (Tru, drop 4 str)
    | kwMatch str "false" = Just (Fals, drop 5 str)
    | kwMatch str "type" = Just (Type, drop 4 str)
    | otherwise = Nothing
 
kwMatch (s:tr) (k:w) = s == k && (kwMatch tr w)
-- matched kw, but does input continue?
kwMatch (s:tr) _ = not (isAlphaNum s)
-- end of input, keyword not complete
kwMatch _ (k:w) = False
kwMatch _ _ = False

-- '' = error
lexChar ('\'' : '\'' : rest) = Nothing
-- '\b'
lexChar ('\'' : '\\' : b : '\'' : rest) = (\x -> (Character x, rest)) <$> (escapeC b)
-- 'a'
lexChar ('\'' : a : '\'': rest) = Just (Character a, rest)
lexChar _ = Nothing

escapeC 'n' = Just '\n'
escapeC '\\' = Just '\\'
escapeC '\'' = Just '\''
escapeC _ = Nothing

lexStr ('"':rest) = (\x -> (String x, (drop num rest))) <$> str
    where (str, num) = getStr rest
lexStr _ = Nothing

-- string
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

escape 'n' = Just '\n'
escape '\\' = Just '\\'
escape '"' = Just '"'
escape _ = Nothing
