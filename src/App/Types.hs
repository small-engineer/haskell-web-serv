{-# LANGUAGE OverloadedStrings #-}

module App.Types
  ( User (..),
  )
where

import Data.Text (Text)

data User = User
  { userName :: Text,
    userPass :: Text
  }
  deriving (Eq, Show)
