{-# LANGUAGE OverloadedStrings #-}

module App.Templates
  ( loginPage,
    registerPage,
    homePage,
    htmlResponse,
  )
where

import App.Types
import Data.Text (Text)
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

homePage :: User -> Html
homePage u =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! A.charset "utf-8"
      H.title "Home"
    H.body $ do
      H.h1 "Home"
      H.p $ do
        H.toHtml ("ログイン中のユーザー: " :: Text)
        H.strong (H.toHtml (userName u))
      H.p $ do
        H.a H.! A.href "/logout" $ "ログアウト"

htmlResponse :: Status -> Html -> Response
htmlResponse st html =
  responseLBS
    st
    [("Content-Type", "text/html; charset=utf-8")]
    (R.renderHtml html)
