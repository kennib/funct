module Language.Funct.AST where

import Data.TypeHash

data Program = Program [Definition] 
    deriving (Show)

data Definition = AliasDefinition Alias Hash
                | TypeDefinition Type
                | FunctionDefinition Alias Function
    deriving (Show)

data Type = SumType [Type]
          | ProductType [Type]
          | TypeHash Hash
          | TypeAlias Alias
    deriving (Show)

data Function = Function [Type] [Value String]
    deriving (Show)

data Alias = Alias String
    deriving (Show)
data Hash = Hash TypeHash
    deriving (Show)
data Value a = Value a
    deriving (Show)
