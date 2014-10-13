module Language.Funct.Parser where

import Control.Monad (liftM, liftM2, liftM3)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (haskellDef, haskellStyle)
import Text.Parsec.Prim (parserReturn)

import Language.Funct.AST

-- Language Definition
functDef = haskellDef
functStyle = haskellStyle

-- Parser
functParser :: Parser Program
functParser = do
    typesHashed        <- many (try $ definitionParser typeAssign                 $ typeParser hash)
    types              <- many (try $ definitionParser typeAssign                 $ typeParser restOfLine)
    functionsHashed    <- many (try $ definitionParser assign                     $ functionParser hash)
    functions          <- many (try $ definitionParser (lookAhead functionAssign) $ functionParser restOfLine)
    _ <- eof

    return $ Program
        typesHashed
        types
        functionsHashed
        functions

definitionParser :: Parser b -> Parser a -> Parser (Definition a)
definitionParser assignParser valueParser = do
    alias' <- alias
    _      <- assignParser
    value  <- valueParser
    _      <- many newline

    return $ Definition alias' value

typeParser :: Parser a -> Parser (Type a)
typeParser = liftM Type

functionParser :: Parser a -> Parser (Function a)
functionParser = liftM Function 

-- Lexer
lexer = makeTokenParser haskellDef

ident = identifier lexer

assign = reservedOp lexer "="
typeAssign = reservedOp lexer "::"
functionAssign = do
    many ident
    assign

restOfLine = manyTill anyChar $ lookAhead (eof <|> (newline >> return ()))
ignoreLine = restOfLine >> return ()

alias :: Parser Alias
alias = liftM Alias $ identifier lexer

hash :: Parser Hash
hash = do
    _ <- char '#'
    hashString <- hex
    return $ Hash $ typeHash hashString

hex = many1 $ oneOf ['a'..'f'] <|> digit

-- Misc.
typeHash s = read $ "TypeHash " ++ show s
