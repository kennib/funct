module Language.Funct.AST where

data Program = Program
    [Definition (Type Hash)]
    [Definition (Type String)]
    [Definition (Function Hash)]
    [Definition (Function String)]
    deriving (Show)

data Definition a = Definition Alias a
    deriving (Show)

data Type a = Type a
    deriving (Show)
data Function a = Function a
    deriving (Show, Read)

data Alias = Alias String
    deriving (Eq, Show)
data Hash = Hash String
    deriving (Eq, Show)
