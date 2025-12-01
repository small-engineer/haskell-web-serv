{-# LANGUAGE OverloadedStrings #-}

module App.Types
  ( User (..),
    Post (..)
  )
where

import Data.Text (Text)

data User = User
  { userName :: Text,
    userPass :: Text
  }
  deriving (Eq, Show)

data Post = Post
  { postId :: Int,
    postAuthor :: Text,
    postBody :: Text,
    postCreatedAt :: Text
  }
  deriving (Eq, Show)
