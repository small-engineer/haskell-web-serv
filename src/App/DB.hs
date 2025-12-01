{-# LANGUAGE OverloadedStrings #-}

module App.DB
  ( initDB
  , withConn
  , findUser
  , createUser
  , listRecentPosts
  , addPost
  , deletePost
  ) where

import App.Types
  ( CreatedAt(..)
  , NonEmptyBody(..)
  , Password(..)
  , Post(..)
  , PostId(..)
  , User(..)
  , UserName(..)
  , formatCreatedAtText
  , mkNonEmptyBody
  , parseCreatedAtText
  , unUserName
  , unPostId
  , unBody
  )
import qualified Crypto.BCrypt as BC
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Database.SQLite.Simple
  ( Connection
  , Only(..)
  , SQLError(..)
  , Error(..)
  , close
  , execute
  , execute_
  , lastInsertRowId
  , open
  , query
  , query_
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Control.Exception (catch)

initDB :: FilePath -> IO ()
initDB dbPath = do
  createDirectoryIfMissing True (takeDirectory dbPath)
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
  close conn

withConn :: FilePath -> (Connection -> IO a) -> IO a
withConn dbPath f = do
  createDirectoryIfMissing True (takeDirectory dbPath)
  conn <- open dbPath
  r <- f conn
  close conn
  pure r

hashPassword :: Password -> IO (Maybe Text)
hashPassword (Password pw) = do
  let raw = TE.encodeUtf8 pw
  m <- BC.hashPasswordUsingPolicy BC.slowerBcryptHashingPolicy raw
  pure (TE.decodeUtf8 <$> m)

validatePassword :: Text -> Password -> Bool
validatePassword hashed (Password pw) =
  BC.validatePassword (TE.encodeUtf8 hashed) (TE.encodeUtf8 pw)

findUser :: Connection -> UserName -> Password -> IO (Maybe User)
findUser conn nm pw = do
  rows <-
    query
      conn
      "SELECT name, password FROM users WHERE name = ?"
      (Only (unUserName nm)) ::
      IO [(Text, Text)]
  case rows of
    [(n, storedHash)] ->
      if validatePassword storedHash pw
        then pure (Just (User (UserName n) (Password "")))
        else pure Nothing
    _ -> pure Nothing

createUser :: Connection -> User -> IO (Either Text ())
createUser conn u = do
  mHash <- hashPassword (userPass u)
  case mHash of
    Nothing ->
      pure (Left "パスワードの保存に失敗しました。もう一度お試しください。")
    Just hp -> do
      r <-
        ( execute
            conn
            "INSERT INTO users (name, password) VALUES (?, ?)"
            (unUserName (userName u), hp)
          >> pure (Right ())
        ) `catch` handler
      pure r
  where
    handler :: SQLError -> IO (Either Text ())
    handler e =
      case sqlError e of
        ErrorConstraint ->
          pure (Left "そのユーザー名は既に使われています。別のユーザー名を指定してください。")
        _ -> do
          putStrLn ("[ERROR] createUser failed: " ++ show e)
          pure (Left "ユーザー登録に失敗しました。時間をおいて再度お試しください。")

listRecentPosts :: Connection -> IO [Post]
listRecentPosts conn = do
  rows <-
    query_
      conn
      "SELECT id, author, body, created_at \
      \FROM posts \
      \ORDER BY id DESC \
      \LIMIT 1000" ::
      IO [(Int, Text, Text, Text)]
  pure (mapMaybe rowToPost rows)
  where
    rowToPost :: (Int, Text, Text, Text) -> Maybe Post
    rowToPost (i, authorTxt, bodyTxt, createdTxt) = do
      body <- mkNonEmptyBody bodyTxt
      created <- parseCreatedAtText createdTxt
      let pid    = PostId i
          author = UserName authorTxt
      pure (Post pid author body created)

addPost :: Connection -> UserName -> NonEmptyBody -> IO Post
addPost conn author body = do
  now <- getCurrentTime
  let created = CreatedAt now
      ts      = formatCreatedAtText created
  execute
    conn
    "INSERT INTO posts (author, body, created_at) VALUES (?, ?, ?)"
    (unUserName author, unBody body, ts)
  rowId <- lastInsertRowId conn
  let pid = PostId (fromIntegral (rowId :: Int64))
  pure (Post pid author body created)

deletePost :: Connection -> PostId -> IO ()
deletePost conn pid =
  execute
    conn
    "DELETE FROM posts WHERE id = ?"
    (Only (unPostId pid))
