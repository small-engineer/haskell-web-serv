{-# LANGUAGE OverloadedStrings #-}

module App.Server
  ( runServer
  ) where

import App.Auth
  ( Token
  , issueToken
  , verifyToken
  )
import App.Board
  ( BoardState
  , boardAllDesc
  , boardInsert
  , boardNewerThan
  , fromPosts
  )
import App.DB
  ( withConn
  , findUser
  , createUser
  , listRecentPosts
  , addPost
  , deletePost
  )
import App.Env
  ( Env(..)
  , AppM(..)
  , runAppM
  )
import App.Templates
  ( loginPage
  , registerPage
  , homePage
  , htmlResponse
  , postsFragment
  )
import App.Types
  ( AuthUser(..)
  , User(..)
  , UserName(..)
  , Password(..)
  , PostId(..)
  , postId
  , mkAuthUser
  , mkNonEmptyBody
  )
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , readTVarIO
  , writeTChan
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
  ( hCookie
  , hLocation
  , status200
  , status302
  , status404
  )
import qualified Network.HTTP.Types as HT
import Network.Wai
  ( Application
  , Request(..)
  , Response
  , responseLBS
  , queryString
  , requestHeaders
  , strictRequestBody
  )
import Network.Wai.Handler.Warp (run)
import Text.Read (reads)

data Route
  = RRoot
  | RLoginGet
  | RLoginPost
  | RRegisterGet
  | RRegisterPost
  | RHomeGet
  | RPostsUpdates
  | RPostsCreate
  | RPostsDelete
  | RLogout
  | RHealth
  | RStyle
  | RNotFound

data AuthResult = AuthResult
  { arUser  :: AuthUser
  , arToken :: Token
  }

runServer :: Env -> Int -> IO ()
runServer env p =
  run p (app env)

app :: Env -> Application
app env req respond = do
  resp <- runAppM env (appHandler req)
  respond resp

appHandler :: Request -> AppM Response
appHandler req =
  case routeOf req of
    RRoot          -> pure (redirectResponse "/login")
    RLoginGet      -> pure (htmlResponse status200 (loginPage Nothing))
    RLoginPost     -> handleLogin req
    RRegisterGet   -> pure (htmlResponse status200 (registerPage Nothing))
    RRegisterPost  -> handleRegister req
    RHomeGet       -> handleHome req
    RPostsUpdates  -> handlePostsUpdates req
    RPostsCreate   -> handleCreatePost req
    RPostsDelete   -> handleDeletePost req
    RLogout        -> pure handleLogout
    RHealth        -> pure healthResponse
    RStyle         -> handleCss
    RNotFound      -> pure notFoundResponse

routeOf :: Request -> Route
routeOf req =
  case (requestMethod req, pathInfo req) of
    ("GET", [])                   -> RRoot
    ("GET", ["login"])            -> RLoginGet
    ("POST", ["login"])           -> RLoginPost
    ("GET", ["register"])         -> RRegisterGet
    ("POST", ["register"])        -> RRegisterPost
    ("GET", ["home"])             -> RHomeGet
    ("GET", ["posts", "updates"]) -> RPostsUpdates
    ("POST", ["posts"])           -> RPostsCreate
    ("POST", ["posts", "delete"]) -> RPostsDelete
    ("GET", ["logout"])           -> RLogout
    ("GET", ["health"])           -> RHealth
    ("GET", ["style.css"])        -> RStyle
    _                             -> RNotFound

handleLogin :: Request -> AppM Response
handleLogin req = do
  body <- liftIO (strictRequestBody req)
  let params = HT.parseSimpleQuery (BL.toStrict body)
  case (lookup "username" params, lookup "password" params) of
    (Just u, Just p) -> do
      let nameTxt = TE.decodeUtf8 u
          passTxt = TE.decodeUtf8 p
          nm      = UserName nameTxt
          pw      = Password passTxt
      env <- ask
      mFound <-
        liftIO $
          withConn (envDbPath env) $ \conn ->
            findUser conn nm pw
      case mFound of
        Nothing ->
          pure (htmlResponse status200 (loginPage (Just "ユーザー名かパスワードが違います。")))
        Just user -> do
          let au = mkAuthUser (userName user)
          tok <- issueToken au
          let setCookieVal =
                B8.concat
                  [ "token="
                  , tok
                  , "; HttpOnly; Path=/; SameSite=Lax"
                  ]
              headers =
                [ (hLocation, "/home")
                , ("Set-Cookie", setCookieVal)
                ]
          pure (responseLBS status302 headers "")
    _ ->
      pure (htmlResponse status200 (loginPage (Just "ユーザー名とパスワードを入力してください。")))

handleRegister :: Request -> AppM Response
handleRegister req = do
  body <- liftIO (strictRequestBody req)
  let params = HT.parseSimpleQuery (BL.toStrict body)
  case (lookup "username" params, lookup "password" params) of
    (Just u, Just p) -> do
      let nameTxt = TE.decodeUtf8 u
          passTxt = TE.decodeUtf8 p
          newUser = User (UserName nameTxt) (Password passTxt)
      env <- ask
      res <-
        liftIO $
          withConn (envDbPath env) $ \conn ->
            createUser conn newUser
      case res of
        Left msg ->
          pure (htmlResponse status200 (registerPage (Just msg)))
        Right _ ->
          pure (htmlResponse status200 (loginPage (Just "登録が完了しました。ログインしてください。")))
    _ ->
      pure (htmlResponse status200 (registerPage (Just "ユーザー名とパスワードを入力してください。")))

handleHome :: Request -> AppM Response
handleHome req = do
  r <- requireAuth req
  case r of
    Left resp -> pure resp
    Right (AuthResult au tok) -> do
      env <- ask
      st  <- liftIO (readTVarIO (envBoardState env))
      let csrfTok = TE.decodeUtf8 tok
          posts   = boardAllDesc st
      pure (htmlResponse status200 (homePage au csrfTok posts))

handlePostsUpdates :: Request -> AppM Response
handlePostsUpdates req = do
  r <- requireAuth req
  case r of
    Left _ -> pure (htmlResponse status200 mempty)
    Right (AuthResult au tok) -> do
      env <- ask
      st  <- liftIO (readTVarIO (envBoardState env))
      let qs       = queryString req
          mAfterBs = lookup "after" qs >>= id
          mPid     = mAfterBs >>= readPostId
          newPosts =
            case mPid of
              Nothing  -> []
              Just pid -> boardNewerThan pid st
          csrfTok  = TE.decodeUtf8 tok
          isAdmin  = authIsAdmin au
      pure (htmlResponse status200 (postsFragment isAdmin csrfTok newPosts))

handleCreatePost :: Request -> AppM Response
handleCreatePost req = do
  r <- requireAuth req
  case r of
    Left resp -> pure resp
    Right (AuthResult au tok) -> do
      body <- liftIO (strictRequestBody req)
      let params = HT.parseSimpleQuery (BL.toStrict body)
          mBody  = lookup "body" params
          mCsrf  = lookup "csrf" params
      case mCsrf of
        Just c | c == tok ->
          case mBody of
            Nothing -> pure (redirectResponse "/home")
            Just b  -> do
              let msgTxt = TE.decodeUtf8 b
              case mkNonEmptyBody msgTxt of
                Nothing     -> pure (redirectResponse "/home")
                Just neBody -> do
                  env <- ask
                  newPost <-
                    liftIO $
                      withConn (envDbPath env) $ \conn ->
                        addPost conn (authUserName au) neBody
                  liftIO $
                    atomically $ do
                      modifyTVar' (envBoardState env) (boardInsert newPost)
                      writeTChan (envBoardChan env) ()
                  pure (redirectResponse "/home")
        _ ->
          pure (redirectResponse "/home")

handleDeletePost :: Request -> AppM Response
handleDeletePost req = do
  r <- requireAuth req
  case r of
    Left resp -> pure resp
    Right (AuthResult au tok) ->
      if not (authIsAdmin au)
        then pure (redirectResponse "/home")
        else do
          body <- liftIO (strictRequestBody req)
          let params = HT.parseSimpleQuery (BL.toStrict body)
              mCsrf  = lookup "csrf" params
              mIdBs  = lookup "id" params
          case (mCsrf, mIdBs) of
            (Just c, Just idBs)
              | c == tok
              , Just pid <- readPostId idBs -> do
                  env <- ask
                  liftIO $
                    withConn (envDbPath env) $ \conn ->
                      deletePost conn pid
                  liftIO $
                    atomically $
                      modifyTVar'
                        (envBoardState env)
                        (\st ->
                           let ps = boardAllDesc st
                               ps' = filter (\p -> postId p /= pid) ps
                           in fromPosts ps'
                        )
                  liftIO $
                    atomically $
                      writeTChan (envBoardChan env) ()
                  pure (redirectResponse "/home")
            _ ->
              pure (redirectResponse "/home")

handleLogout :: Response
handleLogout =
  let expired = "token=; Max-Age=0; Path=/; SameSite=Lax"
      headers =
        [ (hLocation, "/login")
        , ("Set-Cookie", expired)
        ]
   in responseLBS status302 headers ""

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

handleCss :: AppM Response
handleCss = do
  css <- liftIO (BL.readFile "assets/style.css")
  pure
    ( responseLBS
        status200
        [("Content-Type", "text/css; charset=utf-8")]
        css
    )

redirectResponse :: BS.ByteString -> Response
redirectResponse loc =
  let headers = [(hLocation, loc)]
   in responseLBS status302 headers ""

requireAuth :: Request -> AppM (Either Response AuthResult)
requireAuth req =
  case extractTokenFromCookie req of
    Nothing   -> pure (Left (redirectResponse "/login"))
    Just tok  -> do
      mAu <- verifyToken tok
      case mAu of
        Nothing -> pure (Left (redirectResponse "/login"))
        Just au -> pure (Right (AuthResult au tok))

extractTokenFromCookie :: Request -> Maybe Token
extractTokenFromCookie req = do
  raw <- lookup hCookie (requestHeaders req)
  extractToken raw

extractToken :: BS.ByteString -> Maybe Token
extractToken raw =
  let parts  = map B8.strip (B8.split ';' raw)
      predFn bs = B8.isPrefixOf (B8.pack "token=") bs
      mPart  = find predFn parts
   in case mPart of
        Nothing -> Nothing
        Just p  -> Just (B8.drop (B8.length (B8.pack "token=")) p)

readPostId :: B8.ByteString -> Maybe PostId
readPostId bs =
  case reads (B8.unpack bs) of
    [(n, "")] -> Just (PostId n)
    _         -> Nothing
