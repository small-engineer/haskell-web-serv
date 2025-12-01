{-# LANGUAGE OverloadedStrings #-}

module App.Server
  ( runServer,
  )
where

import App.Auth
import App.DB
import App.Templates
import App.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Text as T
import Network.HTTP.Types
  ( hCookie,
    hLocation,
    status200,
    status302,
    status404,
  )
import qualified Network.HTTP.Types as HT
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Internal (ResponseReceived)

runServer :: Int -> IO ()
runServer p = run p app

app :: Application
app req respond = do
  let m = requestMethod req
  let path = pathInfo req
  case (m, path) of
    ("GET", []) ->
      redirectTo "/login" respond
    ("GET", ["login"]) ->
      respond (htmlResponse status200 (loginPage Nothing))
    ("POST", ["login"]) ->
      handleLogin req respond
    ("GET", ["register"]) ->
      respond (htmlResponse status200 (registerPage Nothing))
    ("POST", ["register"]) ->
      handleRegister req respond
    ("GET", ["home"]) ->
      handleHome req respond
    ("GET", ["logout"]) ->
      handleLogout respond
    ("GET", ["health"]) ->
      respond healthResponse
    _ ->
      respond notFoundResponse

handleLogin :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogin req respond = do
  body <- strictRequestBody req
  let params = HT.parseSimpleQuery (BL.toStrict body)
  let mUser = lookup "username" params
  let mPass = lookup "password" params
  case (mUser, mPass) of
    (Just u, Just p) -> do
      let nameTxt = T.pack (B8.unpack u)
      let passTxt = T.pack (B8.unpack p)
      mFound <-
        withConn $ \conn ->
          findUser conn nameTxt passTxt
      case mFound of
        Nothing ->
          respond (htmlResponse status200 (loginPage (Just "ユーザー名かパスワードが違います。")))
        Just user -> do
          tok <- issueToken user
          let setCookieVal = B8.concat ["token=", tok, "; HttpOnly; Path=/"]
          let headers =
                [ (hLocation, "/home"),
                  ("Set-Cookie", setCookieVal)
                ]
          respond (responseLBS status302 headers "")
    _ ->
      respond (htmlResponse status200 (loginPage (Just "ユーザー名とパスワードを入力してください。")))

handleRegister :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleRegister req respond = do
  body <- strictRequestBody req
  let params = HT.parseSimpleQuery (BL.toStrict body)
  let mUser = lookup "username" params
  let mPass = lookup "password" params
  case (mUser, mPass) of
    (Just u, Just p) -> do
      let nameTxt = T.pack (B8.unpack u)
      let passTxt = T.pack (B8.unpack p)
      let newUser = User nameTxt passTxt
      _ <-
        withConn $ \conn ->
          createUser conn newUser
      respond
        ( htmlResponse
            status200
            (loginPage (Just "登録が完了しました。ログインしてください。"))
        )
    _ ->
      respond (htmlResponse status200 (registerPage (Just "ユーザー名とパスワードを入力してください。")))

handleHome :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleHome req respond = do
  mTok <- extractTokenFromCookie req
  case mTok of
    Nothing ->
      redirectTo "/login" respond
    Just tok -> do
      mUser <- verifyToken tok
      case mUser of
        Nothing ->
          redirectTo "/login" respond
        Just u ->
          respond (htmlResponse status200 (homePage u))

handleLogout :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogout respond = do
  let expired = "token=; Max-Age=0; Path=/"
  let headers =
        [ (hLocation, "/login"),
          ("Set-Cookie", expired)
        ]
  respond (responseLBS status302 headers "")

healthResponse :: Response
healthResponse =
  responseLBS
    status200
    [("Content-Type", "text/plain; charset=utf-8")]
    "ok"

notFoundResponse :: Response
notFoundResponse =
  responseLBS
    status404
    [("Content-Type", "text/plain; charset=utf-8")]
    "not found"

redirectTo :: BS.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
redirectTo loc respond = do
  let headers = [(hLocation, loc)]
  respond (responseLBS status302 headers "")

extractTokenFromCookie :: Request -> IO (Maybe Token)
extractTokenFromCookie req = do
  let hs = requestHeaders req
  let mCookie = lookup hCookie hs
  case mCookie of
    Nothing -> return Nothing
    Just raw ->
      return (extractToken raw)

extractToken :: BS.ByteString -> Maybe Token
extractToken raw =
  let parts = map B8.strip (B8.split ';' raw)
      predFn bs = B8.isPrefixOf (B8.pack "token=") bs
      mPart = find predFn parts
   in case mPart of
        Nothing -> Nothing
        Just p -> Just (B8.drop (B8.length (B8.pack "token=")) p)
