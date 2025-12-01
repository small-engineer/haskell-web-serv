{-# LANGUAGE OverloadedStrings #-}

module App.BoardExport
  ( startBoardExporter
  ) where

import App.Env (Env)

startBoardExporter :: Env -> IO ()
startBoardExporter _ =
  pure ()
