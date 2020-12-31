{-# LANGUAGE FlexibleInstances #-}

module Parser.Lexer (AnnotatedToken(..), Token(..), lexFile, passLexer) where 

import Common.Common
import Common.Pass

import Data.List
import Data.Maybe
import Data.Char
import Data.Bifunctor
import Control.Applicative

data Token = LParen | RParen | LSqParen | RSqParen | LCrParen | RCrParen | Integer Int | Character Char | String String | Identifier String
           | Term | Comma | Equals | Return | Yield | PlusEquals | Pipe | New | Void | Type | Colon | Dot | Arrow | If | Then
           | Plus | DoubleEquals | Less | Greater | Minus | Mult | Ampersand | Caret | Tru | Fals | FSlash | BSlash
           | Exclamation | Else | GreaterEqual | LesserEqual | Import | Export | Error deriving (Show, Eq)

{- an token annotated with other data such as line number, characters held -}
data AnnotatedToken = AnnotatedToken Token Int String deriving Eq

instance Disp AnnotatedToken where
    -- convert \n to ; to not mess up output
    disp (AnnotatedToken token@Term ln str) = "; line: " ++ show ln ++ " " ++ show token
    disp (AnnotatedToken token ln str) = str ++ " line: " ++ show ln ++ " " ++ show token
    
instance Disp ([] AnnotatedToken) where
    disp r = intercalate "\n" (map disp r)

passLexer = Pass {pName = ["Lexing"], pFunc = doLx}
    where doLx s = let result = lexFile s in case filterE result of
                                                  [] -> (messageNoLn "Lexer" (disp result) Debug, Just (result))
                                                  es -> (foldr (<>) mempty (map e2Msg es), Nothing)

e2Msg (AnnotatedToken t ln str) = messageLn "Lexer" ("Encountered unknown symbol near " <> show (take 5 str)) Common.Pass.Error ln

filterE = filter (\x -> case x of
                                    (AnnotatedToken Parser.Lexer.Error ln s) -> True
                                    otherwise -> False)
                       
lexFile :: String -> [AnnotatedToken]
lexFile str = lexLine str 1

lexLine :: String -> Int -> [AnnotatedToken]
-- eof
lexLine "" ln = []
-- ignore whitespace
lexLine (' ':tr) ln = lexLine tr ln
lexLine ('/':'/':tr) ln = lexLine (nextLine tr) (ln+1)

-- compare input and output from lexone, update metadata
lexLine str ln = AnnotatedToken token ln missing : lexLine rest newln
    where (token, rest) = lexOne str
          missing = take (length str - length rest) str
          newln = if '\n' `elem` missing then ln + 1 else ln

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
