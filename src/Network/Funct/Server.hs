{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger

import Database.Persist hiding (get)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

import Data.Text.Lazy (pack)

import Data.Time (UTCTime, getCurrentTime)

import Web.Scotty

import Language.Funct.AST
import Language.Funct.Utils (hashFunction)

-- Database
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FunctionDB
    hash String
    function String
    createdAt UTCTime
    HashDB hash
    deriving Show
|]

instance MonadLogger IO where
    monadLoggerLog _ _ _ = pure $ pure ()

runDb query = withSqliteConn "dev.sqlite3" . runSqlConn $ query

getFunction (Hash hash) = getBy $ HashDB hash

setFunction f = do
    let (Hash hash) = hashFunction f
    let (Function function) = f
    now  <- liftIO getCurrentTime
    insert $ FunctionDB hash function now
    return $ Hash hash

-- Server
server = scotty 3000

serveFunctions = do
  get "/function/:hash" $ do
    hash <- param "hash"
    entity <- liftIO $ runDb $ getFunction $ Hash hash
    case entity of
        Just (Entity hash functionDB) -> html $ pack $ show $ Function $ functionDBFunction functionDB
        Nothing -> html "undefined"

receiveFunctions = do
  post "/function" $ do
    function <- param "function"
    (Hash hash) <- liftIO $ runDb $ setFunction (Function  function)
    html $ pack $ show hash

functServer = do
    runDb $ runMigration migrateAll 
    server $ do
        serveFunctions
        receiveFunctions

main :: IO ()
main = do
    functServer
