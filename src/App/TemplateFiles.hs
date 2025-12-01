{-# LANGUAGE OverloadedStrings #-}

module App.TemplateFiles
  ( Templates(..)
  , loadTemplates
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO

data Templates = Templates
  { tplLogin    :: Text
  , tplRegister :: Text
  , tplHome     :: Text
  , tplPostItem :: Text
  }

loadTemplates :: IO Templates
loadTemplates = do
  login <- TIO.readFile "assets/templates/login.html"
  reg   <- TIO.readFile "assets/templates/register.html"
  home  <- TIO.readFile "assets/templates/home.html"
  item  <- TIO.readFile "assets/templates/post_item.html"
  pure
    Templates
      { tplLogin    = login
      , tplRegister = reg
      , tplHome     = home
      , tplPostItem = item
      }
