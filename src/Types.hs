{-# LANGUAGE DeriveGeneric #-}

-- |
--  Module      : Types.hs
--  Description : Used to convert return
module Types
  ( User (..),
    Message (..),
  )
where

import Data.Text
import GHC.Generics

-- -------------------------------------------------------------------------------------------------------------------------------

-- |
-- Created User Datatype
data User = User
  { username :: Int,
    sent_count :: Int,
    outbox :: [Message],
    received_count :: Int,
    inbox :: [Message]
  }
  deriving (Show, Generic)

-- -------------------------------------------------------------------------------------------------------------------------------

-- |
-- Created Message Datatype
data Message = Message
  { source :: Int,
    distination :: Int,
    timestamp :: String,
    content :: String
  }
  deriving (Show)

-- -------------------------------------------------------------------------------------------------------------------------------