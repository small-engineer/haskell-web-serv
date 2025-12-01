{-# LANGUAGE OverloadedStrings #-}

module App.BoardExport
  ( startBoardExporter
  )
where

import App.DB
import App.Types
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
  ( UTCTime
  , defaultTimeLocale
  , parseTimeM
  )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)

boardDir :: FilePath
boardDir = "board"

datDir :: FilePath
datDir = boardDir ++ "/dat"

boardTitle :: T.Text
boardTitle = "mnist-web 掲示板"

startBoardExporter :: IO ()
startBoardExporter = do
  _ <- forkIO loop
  pure ()
  where
    loop = forever $ do
      exportOnce
      threadDelay (5 * 1000000)

exportOnce :: IO ()
exportOnce =
  withConn $ \conn -> do
    posts <- listRecentPosts conn
    let psAsc = sortOn postId posts
    when (not (null psAsc)) $ do
      createDirectoryIfMissing True boardDir
      createDirectoryIfMissing True datDir
      let key = threadKeyFromPosts psAsc
      let datPath = datDir ++ "/" ++ key ++ ".dat"
      let datTxt = buildDatText psAsc
      TIO.writeFile datPath datTxt
      let subjPath = boardDir ++ "/subject.txt"
      let subjLine = buildSubjectLine key boardTitle (length psAsc)
      TIO.writeFile subjPath (subjLine <> T.pack "\n")

threadKeyFromPosts :: [Post] -> String
threadKeyFromPosts [] = "0000000000"
threadKeyFromPosts (p : _) =
  case parseCreatedAt (postCreatedAt p) of
    Nothing  -> "0000000000"
    Just t   ->
      let sec :: Integer
          sec = floor (utcTimeToPOSIXSeconds t)
       in printf "%010d" sec

parseCreatedAt :: T.Text -> Maybe UTCTime
parseCreatedAt t =
  parseTimeM
    True
    defaultTimeLocale
    "%Y-%m-%d %H:%M:%S"
    (T.unpack t)

buildDatText :: [Post] -> T.Text
buildDatText posts =
  let ls = zipWith buildDatLine [0 ..] posts
   in T.intercalate (T.pack "\n") ls

buildDatLine :: Int -> Post -> T.Text
buildDatLine idx p =
  let nm  = escapeHtml (postAuthor p)
      em  = T.empty
      dt  = postCreatedAt p
      bd  = encodeBody (postBody p)
      ttl = if idx == 0 then boardTitle else T.empty
   in T.intercalate
        (T.pack "<>")
        [nm, em, dt, bd, ttl]

encodeBody :: T.Text -> T.Text
encodeBody t =
  let ls = T.splitOn (T.pack "\n") t
      encLine l =
        T.concat
          [ T.pack " "
          , escapeHtml l
          , T.pack " "
          ]
   in T.intercalate
        (T.pack "<br>\n")
        (map encLine ls)

escapeHtml :: T.Text -> T.Text
escapeHtml =
  T.concatMap repl
  where
    repl '<'  = T.pack "&lt;"
    repl '>'  = T.pack "&gt;"
    repl '&'  = T.pack "&amp;"
    repl '"'  = T.pack "&quot;"
    repl c    = T.singleton c

buildSubjectLine :: String -> T.Text -> Int -> T.Text
buildSubjectLine key ttl cnt =
  let namePart =
        T.concat
          [ ttl
          , T.pack " ("
          , T.pack (show cnt)
          , T.pack ")"
          ]
      filePart = T.pack (key ++ ".dat")
   in T.concat
        [ namePart
        , T.pack "\t"
        , filePart
        ]
