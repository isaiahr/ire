module Parser.ParserRels (passParseRels, parseFile) where

import Parser.Lexer
import Parser.ParserCore

import Common.Pass
import Common.Common


import Control.Applicative

{---
parser for import / exports

--}

passParseRels :: Pass [AnnotatedToken] ([String], [String])
passParseRels = Pass {pName = ["Parser"], pFunc = doPs}
    where doPs x = case run parseFile x of
                        ParseSuccess n t -> (mempty, Just n)
                        otherwise -> (messageNoLn "ParserRels" "Error parsing" Common.Pass.Error, Nothing)

parseFile :: Parser ([String], [String])
parseFile = liftA2 (,) parseImports parseExport

parseImports :: Parser [String]
parseImports = (collect (pure ()) (parseToken Term)) *> ((collectM parseImport (parseToken Term)) <|> (pure [])) <* (collect (pure ()) (parseToken Term))

-- import "abcdef"
parseImport :: Parser String
parseImport = parseToken Import *> Parser (\x -> 
    case x of
         (AnnotatedToken (String text) l str):zs -> ParseSuccess text zs
         _ -> ParseFailure)

-- export abc, dce, ueu, etc
parseExport :: Parser [String]
parseExport = (parseToken Export *> collect parseIdentifier (parseToken Comma)) <|> (pure [])


parseIdentifier :: Parser String
parseIdentifier = Parser (\x -> 
    case x of
         (AnnotatedToken (Identifier z) l str):zs -> ParseSuccess z zs
         _ -> ParseFailure)

