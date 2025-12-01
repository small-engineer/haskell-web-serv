{-# LANGUAGE OverloadedStrings #-}

module App.Html
  ( escapeHtml
  ) where

import Data.Text (Text)
import qualified Data.Text as T

escapeHtml :: Text -> Text
escapeHtml =
  T.concatMap repl
  where
    repl '<'  = "&lt;"
    repl '>'  = "&gt;"
    repl '&'  = "&amp;"
    repl '"'  = "&quot;"
    repl c    = T.singleton c
