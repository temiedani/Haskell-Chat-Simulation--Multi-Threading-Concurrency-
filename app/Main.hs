module Main where

-- Compile with -threaded
-- |
--  Module      : Main.hs
--  Description : Imports concurrency.hs and calls all the functions there
--
--  This module  stores all the functionality or the app and is called in main 
--  It has the following main functionality:
--
--  1. Set the number of users to spawn
--  2. Call appropriate function to 
-- 2. Create , Fill and empties the appropriate MVars
import Concurrency
import Control.Concurrent
import Control.Monad
import System.IO

-- -------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "-------------------------------------------"
  putStrLn "  Welcome to the Haskell Chat              "
  putStrLn "  Simulating 100 Messages between 10 Users "
  putStrLn "-------------------------------------------"
  -- Disable buffering on stdout
  hSetBuffering stdout NoBuffering

  -- Number of threads(user) to spawn and total messages to send
  let total_users = 10

  -- Initializing Mvars
  total <- newMVar 0
  choosen <- newEmptyMVar

  -- initialize ten users
  users <- newMVar (map initUser [1 .. total_users])

  -- Generate user threads
  userThread users choosen total [1 .. total_users]

  -- Emptying The Mvar boxes
  choosen <- takeMVar choosen
  myusers <- takeMVar users

  -- Print message metrics for users
  mapM_ messageMetrics myusers

  -- Option to print the messages sent and received by each users
  printMessages myusers

-- -------------------------------------------------------------------------------------------------------------------------------