{-# LANGUAGE OverloadedStrings #-}

module App.DB
  ( withConn,
    findUser,
    createUser,
    listRecentPosts,
    addPost,
    deletePost,
  )
where

import App.Types
import Control.Monad (when)
import qualified Crypto.BCrypt as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    close,
    execute,
    execute_,
    open,
    query,
    query_,
  )
import Database.SQLite.Simple.FromRow
  ( FromRow (..),
    field,
  )
import System.Directory (createDirectoryIfMissing)

dbDir :: FilePath
dbDir = "db"

dbPath :: FilePath
dbPath = dbDir ++ "/mnist-web.db"

instance FromRow Post where
  fromRow =
    Post
      <$> field
      <*> field
      <*> field
      <*> field

withConn :: (Connection -> IO a) -> IO a
withConn f = do
  createDirectoryIfMissing True dbDir
  conn <- open dbPath
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \name TEXT NOT NULL UNIQUE,\
    \password TEXT NOT NULL\
    \)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS posts (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \author TEXT NOT NULL,\
    \body TEXT NOT NULL,\
    \created_at TEXT NOT NULL\
    \)"
  r <- f conn
  close conn
  pure r

hashPassword :: Text -> IO (Maybe Text)
hashPassword pw = do
  let raw = TE.encodeUtf8 pw
  m <- BC.hashPasswordUsingPolicy BC.slowerBcryptHashingPolicy raw
  pure (TE.decodeUtf8 <$> m)

validatePassword :: Text -> Text -> Bool
validatePassword hashed pw =
  BC.validatePassword (TE.encodeUtf8 hashed) (TE.encodeUtf8 pw)

findUser :: Connection -> Text -> Text -> IO (Maybe User)
findUser conn nm pw = do
  rows <-
    query
      conn
      "SELECT name, password FROM users WHERE name = ?"
      (Only nm) ::
      IO [(Text, Text)]
  case rows of
    [(n, storedHash)] ->
      if validatePassword storedHash pw
        then pure (Just (User n ""))
        else pure Nothing
    _ -> pure Nothing

createUser :: Connection -> User -> IO ()
createUser conn u = do
  mHash <- hashPassword (userPass u)
  case mHash of
    Nothing ->
      fail "password hashing failed"
    Just hp ->
      execute
        conn
        "INSERT INTO users (name, password) VALUES (?, ?)"
        (userName u, hp)

listRecentPosts :: Connection -> IO [Post]
listRecentPosts conn =
  query_
    conn
    "SELECT id, author, body, created_at \
    \FROM posts \
    \ORDER BY id DESC \
    \LIMIT 1000"

addPost :: Connection -> Text -> Text -> IO ()
addPost conn author body = do
  now <- getCurrentTime
  let ts =
        T.pack
          ( formatTime
              defaultTimeLocale
              "%Y-%m-%d %H:%M:%S"
              now
          )
  execute
    conn
    "INSERT INTO posts (author, body, created_at) VALUES (?, ?, ?)"
    (author, body, ts)
  counts <-
    query_
      conn
      "SELECT COUNT(*) FROM posts" ::
      IO [Only Int]
  case counts of
    [Only n] -> do
      when (n > 1000) $ do
        let over = n - 1000
        execute
          conn
          "DELETE FROM posts \
          \WHERE id IN (\
          \  SELECT id FROM posts \
          \  ORDER BY id ASC \
          \  LIMIT ?\
          \)"
          (Only over)
    _ -> pure ()

deletePost :: Connection -> Int -> IO ()
deletePost conn pid =
  execute
    conn
    "DELETE FROM posts WHERE id = ?"
    (Only pid)
