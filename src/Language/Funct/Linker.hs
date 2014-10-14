module Language.Funct.Linker where

import Language.Funct.AST

getHashType :: Definition (Type Hash) -> Definition (Type String)
getHashType (Definition alias _) = Definition alias (Type "Int -> Int -> Int")

getHashFunction :: Definition (Function Hash) -> Definition (Function String)
getHashFunction (Definition alias _) = Definition alias (Function "a b = a + b")
