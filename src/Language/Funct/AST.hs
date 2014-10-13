module Language.Funct.AST where

import Data.TypeHash

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
    deriving (Show)

data Alias = Alias String
    deriving (Show)
data Hash = Hash TypeHash
    deriving (Show)
