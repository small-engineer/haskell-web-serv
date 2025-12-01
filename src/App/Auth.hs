{-# LANGUAGE OverloadedStrings #-}

module App.Auth
  ( Token
  , issueToken
  , verifyToken
  , loadJwtSecret
  ) where

import App.Env
  ( AppM
  , Env(..)
  )
import App.Types
  ( AuthUser(..)
  , UserName(..)
  , mkAuthUser
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Environment (lookupEnv)
import qualified Web.JWT as J

type Token = B8.ByteString

tokenTTL :: NominalDiffTime
tokenTTL = 3600

clockLeeway :: NominalDiffTime
clockLeeway = 60

loadJwtSecret :: IO Text
loadJwtSecret = do
  m <- lookupEnv "JWT_SECRET"
  case m of
    Nothing ->
      fail "JWT_SECRET is not set"
    Just s ->
      let t = T.pack s
       in if T.length t < 16
            then fail "JWT_SECRET is too short; use at least 16 characters"
            else pure t

issueToken :: AuthUser -> AppM Token
issueToken au = do
  env <- ask
  now <- liftIO getPOSIXTime

  let secTxt = envJwtSecret env

      mIss = J.stringOrURI "mnist-web"
      UserName nmTxt = authUserName au
      mSub = J.stringOrURI nmTxt
      iat  = J.numericDate now
      expv = J.numericDate (now + realToFrac tokenTTL)

      claims =
        J.JWTClaimsSet
          { J.iss = mIss
          , J.sub = mSub
          , J.aud = Nothing
          , J.exp = expv
          , J.nbf = Nothing
          , J.iat = iat
          , J.jti = Nothing
          , J.unregisteredClaims = mempty
          }

      signer :: J.EncodeSigner
      signer = J.hmacSecret secTxt

      header :: J.JOSEHeader
      header = mempty { J.alg = Just J.HS256 }

      jwtTxt = J.encodeSigned signer header claims

  pure (TE.encodeUtf8 jwtTxt)

verifyToken :: Token -> AppM (Maybe AuthUser)
verifyToken tok = do
  env <- ask
  now <- liftIO getPOSIXTime

  let secTxt = envJwtSecret env

      signer :: J.EncodeSigner
      signer = J.hmacSecret secTxt

      verifier :: J.VerifySigner
      verifier = J.toVerify signer

      mJwt = J.decodeAndVerifySignature verifier (TE.decodeUtf8 tok)

  case mJwt of
    Nothing -> pure Nothing
    Just v  ->
      let cs = J.claims v

          expOk =
            case J.exp cs of
              Nothing -> False
              Just nd ->
                let t :: POSIXTime
                    t = J.secondsSinceEpoch nd
                 in now <= t + realToFrac clockLeeway

          issOk =
            case J.iss cs of
              Nothing   -> False
              Just iss' -> J.stringOrURIToText iss' == T.pack "mnist-web"

          mSubTxt = J.sub cs >>= Just . J.stringOrURIToText

       in if not (expOk && issOk)
            then pure Nothing
            else case mSubTxt of
                   Nothing    -> pure Nothing
                   Just subjT -> pure (Just (mkAuthUser (UserName subjT)))
