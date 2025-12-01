{-# LANGUAGE OverloadedStrings #-}

module App.BoardExport
  ( startBoardExporter
  ) where

import App.Board
  ( boardAllDesc
  )
import App.Env
  ( Env(..)
  )
import App.Types
  ( Post(..)
  , UserName(..)
  , NonEmptyBody(..)
  , CreatedAt(..)
  , formatCreatedAtText
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( atomically
  , readTChan
  , readTVarIO
  )
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)

boardDir :: FilePath
boardDir = "board"

datDir :: FilePath
datDir = boardDir ++ "/dat"

boardTitle :: T.Text
boardTitle = "mnist-web 掲示板"

-- envBoardChan からのイベントを待ち、投稿更新があったタイミングでのみ dat/subject を再生成する
-- 5 秒毎のポーリングではなくイベント駆動なので、無駄な I/O / CPU 消費がない
startBoardExporter :: Env -> IO ()
startBoardExporter env = do
  _ <- forkIO (loop env)
  pure ()

loop :: Env -> IO ()
loop env =
  forever $ do
    atomically (readTChan (envBoardChan env))
    exportOnce env

-- BoardState はメモリ常駐なのでここでは単にTVarを読み出してファイルに書き出すだけ
exportOnce :: Env -> IO ()
exportOnce env = do
  st <- readTVarIO (envBoardState env)
  let psDesc = boardAllDesc st
      psAsc  = reverse psDesc
  case psAsc of
    [] -> pure ()
    _  -> do
      createDirectoryIfMissing True boardDir
      createDirectoryIfMissing True datDir
      let key     = threadKeyFromPosts psAsc
          datPath = datDir ++ "/" ++ key ++ ".dat"
          datTxt  = buildDatText psAsc
      TIO.writeFile datPath datTxt
      let subjPath = boardDir ++ "/subject.txt"
          subjLine = buildSubjectLine key boardTitle (length psAsc)
      TIO.writeFile subjPath (subjLine <> T.pack "\n")

threadKeyFromPosts :: [Post] -> String
threadKeyFromPosts [] = "0000000000"
threadKeyFromPosts (p : _) =
  let CreatedAt t = postCreatedAt p
      sec :: Integer
      sec = floor (utcTimeToPOSIXSeconds t)
   in printf "%010d" sec

buildDatText :: [Post] -> T.Text
buildDatText posts =
  let ls = zipWith buildDatLine [0 ..] posts
   in T.intercalate (T.pack "\n") ls

buildDatLine :: Int -> Post -> T.Text
buildDatLine idx p =
  let UserName nmTxt     = postAuthor p
      NonEmptyBody bdTxt = postBody p
      createdTxt         = formatCreatedAtText (postCreatedAt p)
      nm  = escapeHtml nmTxt
      em  = T.empty
      dt  = createdTxt
      bd  = encodeBody bdTxt
      ttl = if idx == 0 then boardTitle else T.empty
   in T.intercalate
        (T.pack "<>")
        [ nm
        , em
        , dt
        , bd
        , ttl
        ]

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
