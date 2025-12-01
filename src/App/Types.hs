{-# LANGUAGE OverloadedStrings #-}

module App.Types
  ( User(..)
  , UserName(..)
  , Password(..)
  , Post(..)
  , PostId(..)
  , NonEmptyBody(..)
  , CreatedAt(..)
  , AuthUser(..)
  , mkNonEmptyBody
  , formatCreatedAtText
  , parseCreatedAtText
  , mkAuthUser
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( UTCTime
  , defaultTimeLocale
  , formatTime
  , parseTimeM
  )

newtype UserName = UserName { unUserName :: Text }
  deriving (Eq, Ord, Show)

newtype Password = Password { unPassword :: Text }
  deriving (Eq, Show)

newtype PostId = PostId { unPostId :: Int }
  deriving (Eq, Ord, Show)

newtype NonEmptyBody = NonEmptyBody { unBody :: Text }
  deriving (Eq, Show)

newtype CreatedAt = CreatedAt { unCreatedAt :: UTCTime }
  deriving (Eq, Ord, Show)

data User = User
  { userName :: UserName
  , userPass :: Password
  }
  deriving (Eq, Show)

data Post = Post
  { postId        :: PostId
  , postAuthor    :: UserName
  , postBody      :: NonEmptyBody
  , postCreatedAt :: CreatedAt
  }
  deriving (Eq, Show)

data AuthUser = AuthUser
  { authUserName :: UserName
  , authIsAdmin  :: Bool
  }
  deriving (Eq, Show)

mkNonEmptyBody :: Text -> Maybe NonEmptyBody
mkNonEmptyBody t =
  let t' = T.strip t
   in if T.null t'
        then Nothing
        else Just (NonEmptyBody t')

formatCreatedAtText :: CreatedAt -> Text
formatCreatedAtText (CreatedAt t) =
  T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t)

parseCreatedAtText :: Text -> Maybe CreatedAt
parseCreatedAtText t =
  fmap CreatedAt
    ( parseTimeM
        True
        defaultTimeLocale
        "%Y-%m-%d %H:%M:%S"
        (T.unpack t)
        :: Maybe UTCTime
    )

mkAuthUser :: UserName -> AuthUser
mkAuthUser nm =
  AuthUser nm (nm == UserName "Admin")
