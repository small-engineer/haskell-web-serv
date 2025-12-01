{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Server

-- | エントリポイント。HTTP サーバを起動する。
main :: IO ()
main = do
  let p = 8080
  putStrLn ("mnist-web: listening on port " ++ show p)
  runServer p
