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
import qualified Data.Text.Encoding as TE
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
import Text.Read (reads)

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
    ("GET", ["posts", "updates"]) ->
      handlePostsUpdates req respond
    ("POST", ["posts"]) ->
      handleCreatePost req respond
    ("POST", ["posts", "delete"]) ->
      handleDeletePost req respond
    ("GET", ["logout"]) ->
      handleLogout respond
    ("GET", ["health"]) ->
      respond healthResponse
    ("GET", ["style.css"]) ->
      serveCss respond
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
      let nameTxt = TE.decodeUtf8 u
      let passTxt = TE.decodeUtf8 p
      mFound <-
        withConn $ \conn ->
          findUser conn nameTxt passTxt
      case mFound of
        Nothing ->
          respond (htmlResponse status200 (loginPage (Just "ユーザー名かパスワードが違います。")))
        Just user -> do
          tok <- issueToken user
          let setCookieVal =
                B8.concat
                  [ "token=",
                    tok,
                    "; HttpOnly; Path=/; SameSite=Lax"
                  ]
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
      let nameTxt = TE.decodeUtf8 u
      let passTxt = TE.decodeUtf8 p
      let newUser = User nameTxt passTxt
      res <-
        withConn $ \conn ->
          createUser conn newUser
      case res of
        Left msg ->
          respond
            ( htmlResponse
                status200
                (registerPage (Just msg))
            )
        Right _ ->
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
        Just u -> do
          posts <-
            withConn $ \conn ->
              listRecentPosts conn
          let csrfTok = T.pack (B8.unpack tok)
          respond (htmlResponse status200 (homePage u csrfTok posts))

handlePostsUpdates :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePostsUpdates req respond = do
  mTok <- extractTokenFromCookie req
  case mTok of
    Nothing ->
      respond (htmlResponse status200 mempty)
    Just tok -> do
      mUser <- verifyToken tok
      case mUser of
        Nothing ->
          respond (htmlResponse status200 mempty)
        Just u -> do
          posts <-
            withConn $ \conn ->
              listRecentPosts conn
          let qs = queryString req
          let mAfterBs = lookup "after" qs >>= id
          let mAfterId = mAfterBs >>= readInt
          let filtered =
                case mAfterId of
                  Nothing  -> []
                  Just aid -> filter (\p -> postId p > aid) posts
          let csrfTok = T.pack (B8.unpack tok)
          let isAdminUser = userName u == "Admin"
          respond (htmlResponse status200 (postsFragment isAdminUser csrfTok filtered))

handleCreatePost :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleCreatePost req respond = do
  mTok <- extractTokenFromCookie req
  case mTok of
    Nothing ->
      redirectTo "/login" respond
    Just tok -> do
      mUser <- verifyToken tok
      case mUser of
        Nothing ->
          redirectTo "/login" respond
        Just u -> do
          body <- strictRequestBody req
          let params = HT.parseSimpleQuery (BL.toStrict body)
          let mBody = lookup "body" params
          let mCsrf = lookup "csrf" params
          case mCsrf of
            Just c | c == tok -> do
              case mBody of
                Nothing ->
                  redirectTo "/home" respond
                Just b -> do
                  let msgTxt = T.strip (TE.decodeUtf8 b)
                  if T.null msgTxt
                    then redirectTo "/home" respond
                    else do
                      _ <-
                        withConn $ \conn ->
                          addPost conn (userName u) msgTxt
                      redirectTo "/home" respond
            _ ->
              redirectTo "/home" respond

handleDeletePost :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleDeletePost req respond = do
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
          if userName u /= "Admin"
            then redirectTo "/home" respond
            else do
              body <- strictRequestBody req
              let params = HT.parseSimpleQuery (BL.toStrict body)
              let mCsrf = lookup "csrf" params
              let mIdBs = lookup "id" params
              case (mCsrf, mIdBs) of
                (Just c, Just idBs)
                  | c == tok
                  , Just pid <- readInt idBs -> do
                      _ <-
                        withConn $ \conn ->
                          deletePost conn pid
                      redirectTo "/home" respond
                _ ->
                  redirectTo "/home" respond

handleLogout :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleLogout respond = do
  let expired = "token=; Max-Age=0; Path=/; SameSite=Lax"
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

serveCss :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveCss respond = do
  css <- BL.readFile "assets/style.css"
  let resp =
        responseLBS
          status200
          [("Content-Type", "text/css; charset=utf-8")]
          css
  respond resp

redirectTo :: BS.ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived
redirectTo loc respond = do
  let headers = [(hLocation, loc)]
  respond (responseLBS status302 headers "")

extractTokenFromCookie :: Request -> IO (Maybe Token)
extractTokenFromCookie req = do
  let hs = requestHeaders req
  let mCookie = lookup hCookie hs
  case mCookie of
    Nothing -> pure Nothing
    Just raw ->
      pure (extractToken raw)

extractToken :: BS.ByteString -> Maybe Token
extractToken raw =
  let parts = map B8.strip (B8.split ';' raw)
      predFn bs = B8.isPrefixOf (B8.pack "token=") bs
      mPart = find predFn parts
   in case mPart of
        Nothing -> Nothing
        Just p -> Just (B8.drop (B8.length (B8.pack "token=")) p)

readInt :: B8.ByteString -> Maybe Int
readInt bs =
  case reads (B8.unpack bs) of
    [(n, "")] -> Just n
    _         -> Nothing
