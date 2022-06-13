{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Concurrency(initUser, generateRandomMessage, getRandomReceiver, isNumeric, updateUser, userThread, sendMessage, messageMetrics, printMessages) where

-- Compile with -threaded
-- |
--  Module      : Concurrency.hs
--  Description : A module implementing the concepts of concurrency
--
--  This module  stores all the functionality or the app and is called in main 
--  It has the following main functionality:
--
--  1. Initialize new users 
--  2. Create new Threads
--  3. Send messages between each thread
--  4. Save Messages to database and local text file
--  5. Print User messages
import Control.Concurrent
import Control.Monad
import Data.Char (isDigit, isLetter)
import Data.Text (pack)
import Data.Time
import Database
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Random
import Text.StringRandom
import Types

-- -------------------------------------------------------------------------------------------------------------------------------
-- |
-- Initialize new User with empty inbox and outbox and zero 
initUser :: Int -> User
initUser username = User username 0 [] 0 []

-- -------------------------------------------------------------------------------------------------------------------------------
-- |
-- Generate messages from text file corpus
generateRandomMessage :: Int -> IO String
generateRandomMessage index = do
  contents <- readFile "chat.txt"
  let sentences = lines contents
  return $! sentences !! index

---------------------------------------------------------------------------------------------------------------------------------

-- |
-- Returns a new receiver user diffrent from sender
getRandomReceiver :: Int -> IO Int
getRandomReceiver sender = do
  receiver <- randomRIO (1, 10) -- generate random int between 1 and 10
  if receiver == sender
    then getRandomReceiver sender
    else do
      return receiver

-- -------------------------------------------------------------------------------------------------------------------------------

-- |
--  Checks if a string contains only numeric characters
isNumeric ::
  -- | User input
  [Char] ->
  -- | True if the user enters only characters which are numbers and False otherwise
  Bool
isNumeric [] = True
isNumeric (a : as) = if (isDigit a) then isNumeric as else False

-- -------------------------------------------------------------------------------------------------------------------------------

-- |
-- Increments messages_received, inserts message into inbox of 
updateUser :: User -> Message -> User
updateUser user message
    -- if sender add message to outbox sent and increment sent_count
  | username user == source message = user {sent_count = (sent_count user) + 1, outbox = message : outbox user} 
    -- if receiver add message to inbox and increment received_count
  | username user == distination message = user {received_count = (received_count user) + 1, inbox = message : inbox user} 
  | otherwise = user

-- -------------------------------------------------------------------------------------------------------------------------------

-- |
-- Create User Threads and call the send message functions for each thread generated
userThread :: MVar [User] -> MVar Int -> MVar Int -> [Int] -> IO ()
userThread users choosen total thread_ids = do
  forM_ thread_ids $ \thread -> -- for each number 0 to 10
    forkIO (sendMessage thread users choosen total)

-- -------------------------------------------------------------------------------------------------------------------------------

-- |
-- Sending message for each thread randomly and checks if the total message sent are not more than 100
sendMessage :: Int -> MVar [User] -> MVar Int -> MVar Int -> IO ()
sendMessage username users choosen total = do
  let total_messages = 100 -- Initialize how many messages to simulate
  tid <- myThreadId -- Get thread id and convert to string
  totalSent <- takeMVar total -- Get total message sent so far
  myuser <- takeMVar users -- Get list of users to send message to
  if totalSent < total_messages
    then do
      -- get random user to send message to
      receiver <- getRandomReceiver username
      -- record times od sending message
      timestamp <- getCurrentTime
      --genrate random string message
      randMessage <- generateRandomMessage totalSent
      putStrLn "-------------------------------------------"
      putStrLn $ "Running user at: " ++ show tid
      putStrLn $ "From User: " ++ show username
      putStrLn $ "To User: " ++ show receiver
      putStrLn $ "Message: " ++ show randMessage
      putStrLn $ "Timestamp: " ++ show timestamp
      putStrLn "-------------------------------------------"
      -- Saving Messages into local text file
      putStrLn "Saving Messages to Text File..."
      let file = "messages.txt"
      let output = show tid ++ "\nFrom User: " ++ show username ++ "\nTo User: " ++ show receiver ++ "\nMessage: " ++ randMessage ++ "\nTimestamp: " ++ show timestamp
      writeFile file output
      -- Saving Messages into Database
      conn <- initialiseDB
      saveMessages conn username receiver randMessage (show timestamp) (show tid)
      --
      let message = Message {source = username, distination = receiver, timestamp = "t", content = randMessage}
      let updatedUsers = map updater myuser
          updater user = updateUser user message
      -- increment total messages sent
      let totalPlus = totalSent + 1
      putMVar total totalPlus
      putMVar users updatedUsers
      rdn <- randomRIO (5, 15) -- generate random int between 5 and 15
      threadDelay rdn
      sendMessage username users choosen total
    else do
      print "100 Messages Sent!! Terminating..."
      putStrLn "-------------------------------------------"
      print "Simulation Summary..."
      -- fill Mvars and block the next threads
      putMVar choosen username
      putMVar users myuser
      return ()

-- -------------------------------------------------------------------------------------------------------------------------------
-- |
-- Print how many message each user have received or Sent
messageMetrics :: User -> IO ()
messageMetrics user = do
  let userOutput = "users.txt"
  -- Saving user to a local text file
  writeFile userOutput $ "User: " ++ (show $ username user) ++ "\nOutbox Count: " ++ (show $ sent_count user) ++ "\nOutbox: " ++ (show $ outbox user) ++ "\nInbox Count: " ++ (show $ received_count user) ++ "\nInbox: " ++ (show $ inbox user)

  -- Printing Summary of Simulation
  putStrLn $ "USER " ++ (show $ username user) ++ " SENT " ++ (show $ sent_count user) ++ " messages and RECEIVED " ++ (show $ received_count user) ++ " Messages"

-- -------------------------------------------------------------------------------------------------------------------------------
-- |
-- Prints The Inbox and Outbox of a user given the username of a user
printMessages :: [User] -> IO ()
printMessages users = do
  putStrLn "---------------------------------------"
  putStrLn "Choose an option of Users to print>    "
  putStrLn " (From 1 to 10 ) to print user messages"
  putStrLn " Enter any other number to exit        "
  putStrLn "---------------------------------------"
  option <- getLine
  --option <- readLn :: IO Int
  let option' = if isNumeric option then option else "Not numeric"
  case option' of
    "" -> do
      print "Error!!, Empty Input, Exiting the program... "
      exitFailure
    "Not numeric" -> do
      print "Please enter a Numeric Option!"
      printMessages users
    otherwise -> do
      -- Convert User input into integer
      let index = read option' :: Int
      -- check if in range of [1..10]
      if (index > 0 && index < 11)
        then do
          -- Get user from array of users
          let u = users !! (index -1)
          putStrLn $ "Printing messases for User: " ++ (show $ username u)
          putStrLn $ "Outbox Count: " ++ (show $ sent_count u)
          putStrLn $ "Outbox: " ++ (show $ outbox u)
          putStrLn $ "Inbox Count: " ++ (show $ received_count u)
          putStrLn $ "Inbox: " ++ (show $ inbox u)
          -- Enter a loop
          printMessages users
        else do
          -- Exit the  Program
          print "User does not Exist!"
          print "Exiting the program..."
          exitSuccess
-- -------------------------------------------------------------------------------------------------------------------------------