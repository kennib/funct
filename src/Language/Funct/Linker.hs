module Language.Funct.Linker where

import Language.Funct.AST
import Network.Funct

getHashType :: Definition (Type Hash) -> Definition (Type String)
getHashType (Definition alias _) = Definition alias (Type "Int -> Int -> Int")

getHashFunction :: Definition (Function Hash) -> IO (Definition (Function String))
getHashFunction (Definition alias (Function hash)) = do
    function <- getFunction hash
    return $ Definition alias function
