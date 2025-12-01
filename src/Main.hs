{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Server
import App.BoardExport

main :: IO ()
main = do
  startBoardExporter
  let p = 8080
  putStrLn ("mnist-web: listening on port " ++ show p)
  runServer p
