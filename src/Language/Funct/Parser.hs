module Language.Funct.Parser where

import Control.Monad (liftM, liftM2, liftM3)
import Text.ParserCombinators.Parsec hiding (space, spaces)

import Language.Funct.AST

functParse :: String -> Either ParseError Program
functParse = parse programParser "Funct"

programParser :: Parser Program
programParser = liftM Program $
    withSpaces definitionParser `sepBy` newline
    `ignore` eof

definitionParser :: Parser Definition
definitionParser = (try aliasDefinitionParser)
         <|> (try functionDefinitionParser)
         <|> (try $ liftM TypeDefinition typeParser)


aliasDefinitionParser :: Parser Definition
aliasDefinitionParser = liftM2 AliasDefinition
    (withSpaces aliasParser)
    hashParser

aliasParser :: Parser Alias
aliasParser = liftM Alias
    (many1 letter)

hashParser :: Parser Hash
hashParser = liftM Hash $
    liftM typeHash (many1 hex)
    where hex = oneOf ['a'..'f'] <|> digit


typeParser :: Parser Type
typeParser = try typeAliasParser
         <|> try typeHashParser
         <|> try sumTypeParser
         <|> try productTypeParser

typeAliasParser :: Parser Type
typeAliasParser = liftM TypeAlias
    aliasParser

typeHashParser :: Parser Type
typeHashParser = liftM TypeHash
    hashParser

sumTypeParser :: Parser Type
sumTypeParser = liftM SumType $
   (parentheses typeParser) `sepBy1` (withSpaces $ char '|')

productTypeParser :: Parser Type
productTypeParser = liftM ProductType $
   (parentheses typeParser) `sepBy1` spaces


functionDefinitionParser :: Parser Definition
functionDefinitionParser = do 
    (alias, values) <- functionBodyParser
    (alias, types) <- functionTypeParser
    let function = Function types values

    return $ FunctionDefinition alias function

functionTypeParser :: Parser (Alias, [Type])
functionTypeParser = liftM2 (,)
    (withSpaces aliasParser `ignore` withSpaces (string "::"))
    (withSpaces typeParser
    `sepBy` withSpaces (string "->")
    `ignore` newline)

functionBodyParser :: Parser (Alias, [Value String])
functionBodyParser = liftM2 (,)
    (withSpaces aliasParser `ignore` withSpaces (char '='))
    (valueParser `sepBy` spaces)

valueParser :: Parser (Value String)
valueParser = liftM Value $
    many1 letter


withSpaces = between (optional spaces) (optional spaces)
spaces = skipMany space
space = char ' '

parentheses  = between (char '(') (char ')')

ignore p1 p2 = do
    pa <- p1
    pb <- p2
    return pa
ignoreThen p1 p2 = p1 >> p2

typeHash s = read $ "TypeHash " ++ show s
