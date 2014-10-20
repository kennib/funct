module Network.Funct where

import Network.HTTP

import Language.Funct.AST

getFunction :: Hash -> IO (Function String)
getFunction (Hash hash) = do
    response <- simpleHTTP $ getRequest $ "http://localhost:3000/function/" ++ hash
    function <- getResponseBody response

    return $ read function 

giveFunction :: Function String -> IO ()
giveFunction (Function function) = do
    simpleHTTP $ postRequest $ "http://localhost:3000/function?function=" ++ function 
    return ()
