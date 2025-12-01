{-# LANGUAGE OverloadedStrings #-}

module App.Auth
  ( Token,
    issueToken,
    verifyToken,
  )
where

import App.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

type Token = B8.ByteString

secretPrefix :: B8.ByteString
secretPrefix = B8.pack "dev-token-"

issueToken :: User -> IO Token
issueToken u = do
  let nm = userName u
  let t = B8.append secretPrefix (B8.pack (T.unpack nm))
  return t

verifyToken :: Token -> IO (Maybe User)
verifyToken tok =
  if B8.isPrefixOf secretPrefix tok
    then do
      let rest = B8.drop (B8.length secretPrefix) tok
      let nm = T.pack (B8.unpack rest)
      return (Just (User nm ""))
    else return Nothing
