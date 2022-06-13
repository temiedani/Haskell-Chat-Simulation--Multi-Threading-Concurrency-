{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--  Module      : Database.hs
--  Description : Used for performing queries on the sqlite database
--
--  This module is used for performing queries on the sqlite database.
--  It has the following functionality:
--
--  1. Creating table and initializing database connection
--  2. Inserting each message sent between the users
module Database (initialiseDB, saveMessages) where

import Control.Applicative
import qualified Data.Char
import Data.Dynamic
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Types

-- ---------------------------------------------------------------------------------------------------------------------------------

-- |
--  Creates my message table in sqlite database (HaskellChat/messages.sqlite)
initialiseDB :: IO Connection
initialiseDB = do
  conn <- open "messages.sqlite"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS messages (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \sender INT DEFAULT NULL,\
    \receiver INT DEFAULT  NULL,\
    \message VARCHAR(120)  NULL,\
    \timestamp VARCHAR(50) NULL,\
    \thread_id VARCHAR(10) NULL\
    \)"
  return conn

-- ---------------------------------------------------------------------------------------------------------------------------------

-- |
--  Saves new message when called with the content of message
saveMessages :: Connection -> Int -> Int -> String -> String -> String -> IO ()
saveMessages conn from to text time thread = do
  putStrLn "Saving Message to DB..."
  execute conn "INSERT INTO messages (sender, receiver, message, timestamp, thread_id ) VALUES (?, ?, ?, ?, ?)" (from, to, text, time, thread)

-- ---------------------------------------------------------------------------------------------------------------------------------
