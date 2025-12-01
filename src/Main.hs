{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Board (fromPosts)
import App.BoardExport (startBoardExporter)
import App.DB (withConn, listRecentPosts)
import App.Env (Env(..))
import App.Auth (loadJwtSecret)
import App.Server (runServer)
import App.TemplateFiles (loadTemplates)
import Control.Concurrent.STM (newTChanIO, newTVarIO)

main :: IO ()
main = do
  secret <- loadJwtSecret
  tpls   <- loadTemplates

  let dbPath = "db/mnist-web.db"

  posts <- withConn dbPath listRecentPosts
  let st0 = fromPosts posts

  stVar <- newTVarIO st0
  ch    <- newTChanIO

  let env =
        Env
          { envDbPath     = dbPath
          , envJwtSecret  = secret
          , envBoardState = stVar
          , envBoardChan  = ch
          , envTemplates  = tpls
          }
      port = 8080

  startBoardExporter env
  putStrLn ("mnist-web: listening on port " ++ show port)
  runServer env port
