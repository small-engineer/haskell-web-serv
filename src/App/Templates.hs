{-# LANGUAGE OverloadedStrings #-}

module App.Templates
  ( loginPage
  , registerPage
  , homePage
  , htmlResponse
  , postsFragment
  ) where

import App.Types
  ( AuthUser(..)
  , UserName(..)
  , Post(..)
  , PostId(..)
  , NonEmptyBody(..)
  , formatCreatedAtText
  )
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (Status)
import Network.Wai (Response, responseLBS)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

loginPage :: Maybe Text -> Html
loginPage err =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! A.charset "utf-8"
      H.title "Login"
      H.link
        H.! A.rel "stylesheet"
        H.! A.href "/style.css"
    H.body $ do
      H.h1 "Login"
      case err of
        Nothing ->
          H.span H.! A.style "color: #666;" $
            "ユーザー名とパスワードを入力してください。"
        Just msg ->
          H.div H.! A.style "color: red; margin-bottom: 1rem;" $
            H.toHtml msg
      H.form
        H.! A.method "post"
        H.! A.action "/login" $ do
          H.div $ do
            H.label H.! A.for "username" $ "ユーザー名"
            H.input
              H.! A.type_ "text"
              H.! A.name "username"
              H.! A.id "username"
          H.div $ do
            H.label H.! A.for "password" $ "パスワード"
            H.input
              H.! A.type_ "password"
              H.! A.name "password"
              H.! A.id "password"
          H.div H.! A.style "margin-top: 1rem;" $ do
            H.button
              H.! A.type_ "submit" $
              "ログイン"
      H.p H.! A.style "margin-top: 1rem;" $ do
        H.a H.! A.href "/register" $ "ユーザー登録はこちら"

registerPage :: Maybe Text -> Html
registerPage err =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! A.charset "utf-8"
      H.title "Register"
      H.link
        H.! A.rel "stylesheet"
        H.! A.href "/style.css"
    H.body $ do
      H.h1 "Register"
      case err of
        Nothing ->
          H.span H.! A.style "color: #666;" $
            "新しいユーザー名とパスワードを入力してください。"
        Just msg ->
          H.div H.! A.style "color: red; margin-bottom: 1rem;" $
            H.toHtml msg
      H.form
        H.! A.method "post"
        H.! A.action "/register" $ do
          H.div $ do
            H.label H.! A.for "username" $ "ユーザー名"
            H.input
              H.! A.type_ "text"
              H.! A.name "username"
              H.! A.id "username"
          H.div $ do
            H.label H.! A.for "password" $ "パスワード"
            H.input
              H.! A.type_ "password"
              H.! A.name "password"
              H.! A.id "password"
          H.div H.! A.style "margin-top: 1rem;" $ do
            H.button
              H.! A.type_ "submit" $
              "登録"
      H.p H.! A.style "margin-top: 1rem;" $ do
        H.a H.! A.href "/login" $ "ログイン画面へ戻る"

homePage :: AuthUser -> Text -> [Post] -> Html
homePage u csrfTok posts =
  H.docTypeHtml $ do
    let isAdminUser = authIsAdmin u
    H.head $ do
      H.meta H.! A.charset "utf-8"
      H.title "Home"
      H.link
        H.! A.rel "stylesheet"
        H.! A.href "/style.css"
      H.script
        H.! A.type_ "text/javascript" $
        H.toHtml (autoReloadJs :: Text)
    H.body $ do
      H.h1 "Home"
      H.p $ do
        H.toHtml ("ログイン中のユーザー: " :: Text)
        let UserName nm = authUserName u
        H.strong (H.toHtml nm)
      H.p $ do
        H.a H.! A.href "/logout" $ "ログアウト"
      H.hr
      H.h2 "掲示板"
      H.form
        H.! A.method "post"
        H.! A.action "/posts" $ do
          H.input
            H.! A.type_ "hidden"
            H.! A.name "csrf"
            H.! A.value (H.toValue csrfTok)
          H.div $ do
            H.label H.! A.for "body" $ "メッセージ"
            H.br
            H.textarea
              H.! A.name "body"
              H.! A.id "body"
              H.! A.rows "3"
              H.! A.cols "60" $
              ""
          H.div H.! A.style "margin-top: 0.5rem;" $ do
            H.button
              H.! A.type_ "submit" $
              "投稿"
      H.hr
      H.ul
        H.! A.id "posts" $
        mapM_ (renderPost isAdminUser csrfTok) posts

htmlResponse :: Status -> Html -> Response
htmlResponse st html =
  responseLBS
    st
    [("Content-Type", "text/html; charset=utf-8")]
    (R.renderHtml html)

postsFragment :: Bool -> Text -> [Post] -> Html
postsFragment isAdminUser csrfTok ps =
  mapM_ (renderPost isAdminUser csrfTok) ps

renderPost :: Bool -> Text -> Post -> H.Html
renderPost isAdminUser csrfTok p =
  let PostId pid           = postId p
      UserName authorTxt   = postAuthor p
      NonEmptyBody bodyTxt = postBody p
      createdTxt           = formatCreatedAtText (postCreatedAt p)
   in H.li
        H.! A.style "margin-bottom: 0.5rem;"
        H.! H.dataAttribute "id" (H.toValue pid) $ do
             H.div $ do
               H.strong (H.toHtml authorTxt)
               H.toHtml (" さん " :: Text)
               H.span
                 H.! A.style "color: #888; font-size: 0.8rem; margin-left: 0.5rem;" $
                 H.toHtml createdTxt
             H.div $
               H.toHtml bodyTxt
             if isAdminUser
               then do
                 H.form
                   H.! A.method "post"
                   H.! A.action "/posts/delete"
                   H.! A.style "margin-top: 0.25rem;" $ do
                     H.input
                       H.! A.type_ "hidden"
                       H.! A.name "csrf"
                       H.! A.value (H.toValue csrfTok)
                     H.input
                       H.! A.type_ "hidden"
                       H.! A.name "id"
                       H.! A.value (H.toValue pid)
                     H.button
                       H.! A.type_ "submit"
                       H.! A.style "font-size: 0.8rem; color: #c00;" $
                       "削除"
               else pure ()

autoReloadJs :: Text
autoReloadJs =
  T.unlines
    [ "(function(){"
    , "  function getLastId(){"
    , "    var ul = document.getElementById('posts');"
    , "    if(!ul) return 0;"
    , "    var first = ul.querySelector('li[data-id]');"
    , "    if(!first) return 0;"
    , "    var v = first.getAttribute('data-id');"
    , "    var n = parseInt(v, 10);"
    , "    if(isNaN(n)) return 0;"
    , "    return n;"
    , "  }"
    , "  function poll(){"
    , "    var ul = document.getElementById('posts');"
    , "    if(!ul) return;"
    , "    var lastId = getLastId();"
    , "    var xhr = new XMLHttpRequest();"
    , "    xhr.open('GET', '/posts/updates?after=' + lastId, true);"
    , "    xhr.onreadystatechange = function(){"
    , "      if(xhr.readyState !== 4) return;"
    , "      if(xhr.status !== 200) return;"
    , "      if(!xhr.responseText) return;"
    , "      var tmp = document.createElement('div');"
    , "      tmp.innerHTML = xhr.responseText;"
    , "      var items = tmp.querySelectorAll('li');"
    , "      if(!items || !items.length) return;"
    , "      for(var i = items.length - 1; i >= 0; i--){"
    , "        ul.insertBefore(items[i], ul.firstChild);"
    , "      }"
    , "    };"
    , "    xhr.send(null);"
    , "  }"
    , "  setInterval(poll, 5000);"
    , "})();"
    ]
