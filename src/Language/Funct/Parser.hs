module Language.Funct.Parser where

import Control.Monad (liftM, liftM2, liftM3, liftM4)
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
functParser = applyTill defParser (Program [] [] [] []) eof

defParser :: Parser Program -> Parser Program
defParser program = do
    Program typesHashed types functionsHashed functions <- program

    typesHashed'     <- option typesHashed     (liftM2 append (try typeHashed)         $ return typesHashed)
    types'           <- option types           (liftM2 append (try typeDefinition)     $ return types)
    functionsHashed' <- option functionsHashed (liftM2 append (try functionHashed)     $ return functionsHashed)
    functions'       <- option functions       (liftM2 append (try functionDefinition) $ return functions)

    return $ Program typesHashed' types' functionsHashed' functions'

    where append x xs = xs ++ [x]

typeHashed         = definitionParser typeAssign                 $ typeParser hash
typeDefinition     = definitionParser typeAssign                 $ typeParser restOfLine
functionHashed     = definitionParser assign                     $ functionParser hash
functionDefinition = definitionParser (lookAhead functionAssign) $ functionParser restOfLine

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
    return $ Hash hashString

hex = many1 $ oneOf ['a'..'f'] <|> digit

-- Misc.
applyTill :: (Parser a -> Parser a) -> a -> Parser b -> Parser a
applyTill apply base end =
        (do try end; return base)
    <|> do
        base' <- apply $ return base
        applyTill apply base' end
