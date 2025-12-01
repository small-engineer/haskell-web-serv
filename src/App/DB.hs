{-# LANGUAGE OverloadedStrings #-}

module App.DB
  ( withConn,
    findUser,
    createUser,
  )
where

import App.Types
import Data.Text (Text)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing)

dbDir :: FilePath
dbDir = "db"

dbPath :: FilePath
dbPath = dbDir ++ "/mnist-web.db"

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
  r <- f conn
  close conn
  return r

findUser :: Connection -> Text -> Text -> IO (Maybe User)
findUser conn nm pw = do
  rows <-
    query
      conn
      "SELECT name, password FROM users WHERE name = ? AND password = ?"
      (nm, pw) ::
      IO [(Text, Text)]
  case rows of
    [(n, p)] -> return (Just (User n p))
    _ -> return Nothing

createUser :: Connection -> User -> IO ()
createUser conn u =
  execute
    conn
    "INSERT INTO users (name, password) VALUES (?, ?)"
    (userName u, userPass u)
