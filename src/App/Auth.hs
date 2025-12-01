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
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv)
import qualified Web.JWT as J

type Token = B8.ByteString

tokenTTL :: NominalDiffTime
tokenTTL = 3600

clockLeeway :: NominalDiffTime
clockLeeway = 60

getSecretText :: IO T.Text
getSecretText = do
  m <- lookupEnv "JWT_SECRET"
  case m of
    Nothing ->
      fail "JWT_SECRET is not set"
    Just s ->
      let t = T.pack s
       in if T.length t < 16
            then fail "JWT_SECRET is too short; use at least 16 characters"
            else pure t

issueToken :: User -> IO Token
issueToken u = do
  secTxt <- getSecretText
  now    <- getPOSIXTime

  let mIss = J.stringOrURI "mnist-web"
  let mSub = J.stringOrURI (userName u)
  let iat  = J.numericDate now
  let expv = J.numericDate (now + tokenTTL)

  let claims =
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

  let signer :: J.EncodeSigner
      signer = J.hmacSecret secTxt

  let header :: J.JOSEHeader
      header = mempty { J.alg = Just J.HS256 }

  let jwtTxt = J.encodeSigned signer header claims

  pure (TE.encodeUtf8 jwtTxt)

verifyToken :: Token -> IO (Maybe User)
verifyToken tok = do
  secTxt <- getSecretText
  now    <- getPOSIXTime

  let signer :: J.EncodeSigner
      signer = J.hmacSecret secTxt

  let verifier :: J.VerifySigner
      verifier = J.toVerify signer

  let mJwt = J.decodeAndVerifySignature verifier (TE.decodeUtf8 tok)

  case mJwt of
    Nothing -> pure Nothing
    Just v  ->
      let cs = J.claims v

          expOk =
            case J.exp cs of
              Nothing -> False
              Just nd ->
                let t = J.secondsSinceEpoch nd
                 in now <= t + clockLeeway

          issOk =
            case J.iss cs of
              Nothing   -> False
              Just iss' -> J.stringOrURIToText iss' == T.pack "mnist-web"

          mSubTxt = J.sub cs >>= Just . J.stringOrURIToText

      in if not (expOk && issOk)
           then pure Nothing
           else case mSubTxt of
                  Nothing -> pure Nothing
                  Just nm -> pure (Just (User nm ""))
