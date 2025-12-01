{-# LANGUAGE OverloadedStrings #-}

module App.Templates
  ( loginPage
  , registerPage
  , homePage
  , htmlResponse
  , postsFragment
  ) where

import App.TemplateFiles
  ( Templates(..)
  )
import App.Types
  ( AuthUser(..)
  , UserName(..)
  , Post(..)
  , PostId(..)
  , NonEmptyBody(..)
  , formatCreatedAtText
  )
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (Status)
import Network.Wai (Response, responseLBS)

-- Blazeを使わずTextベースにすることでテンプレートのキャッシュと置換処理を純粋関数に
htmlResponse :: Status -> Text -> Response
htmlResponse st txt =
  responseLBS
    st
    [("Content-Type", "text/html; charset=utf-8")]
    (BL.fromStrict (TE.encodeUtf8 txt))

loginPage :: Templates -> Maybe Text -> Text
loginPage tpls mErr =
  let base = tplLogin tpls
      msg =
        case mErr of
          Nothing ->
            "<span style=\"color: #666;\">ユーザー名とパスワードを入力してください。</span>"
          Just e  ->
            T.concat
              [ "<div style=\"color: red; margin-bottom: 1rem;\">"
              , escapeHtml e
              , "</div>"
              ]
   in sub [("ERROR_BLOCK", msg)] base

registerPage :: Templates -> Maybe Text -> Text
registerPage tpls mErr =
  let base = tplRegister tpls
      msg =
        case mErr of
          Nothing ->
            "<span style=\"color: #666;\">新しいユーザー名とパスワードを入力してください。</span>"
          Just e  ->
            T.concat
              [ "<div style=\"color: red; margin-bottom: 1rem;\">"
              , escapeHtml e
              , "</div>"
              ]
   in sub [("ERROR_BLOCK", msg)] base

homePage :: Templates -> AuthUser -> Text -> [Post] -> Text
homePage tpls u csrfTok posts =
  let base      = tplHome tpls
      UserName nm = authUserName u
      postsHtml = postsFragment tpls (authIsAdmin u) csrfTok posts
   in sub
        [ ("USERNAME", escapeHtml nm)
        , ("CSRF", escapeHtml csrfTok)
        , ("POSTS", postsHtml)
        , ("AUTO_RELOAD_JS", autoReloadJs)
        ]
        base

-- BoardState から取り出した [Post] を直に描画
postsFragment :: Templates -> Bool -> Text -> [Post] -> Text
postsFragment tpls isAdmin csrfTok ps =
  T.concat (map (renderPostItem tpls isAdmin csrfTok) ps)

renderPostItem :: Templates -> Bool -> Text -> Post -> Text
renderPostItem tpls isAdmin csrfTok p =
  let PostId pid           = postId p
      UserName authorTxt   = postAuthor p
      NonEmptyBody bodyTxt = postBody p
      createdTxt           = formatCreatedAtText (postCreatedAt p)
      base                 = tplPostItem tpls
      idTxt                = T.pack (show pid)
      delHtml =
        if isAdmin
          then
            T.concat
              [ "<form method=\"post\" action=\"/posts/delete\" style=\"margin-top: 0.25rem;\">"
              , "<input type=\"hidden\" name=\"csrf\" value=\""
              , escapeHtml csrfTok
              , "\">"
              , "<input type=\"hidden\" name=\"id\" value=\""
              , escapeHtml idTxt
              , "\">"
              , "<button type=\"submit\" style=\"font-size: 0.8rem; color: #c00;\">削除</button>"
              , "</form>"
              ]
          else T.empty
   in sub
        [ ("ID", idTxt)
        , ("AUTHOR", escapeHtml authorTxt)
        , ("BODY", escapeHtml bodyTxt)
        , ("CREATED", escapeHtml createdTxt)
        , ("ADMIN_DELETE", delHtml)
        ]
        base

-- 簡易テンプレートエンジン。
sub :: [(Text, Text)] -> Text -> Text
sub kvs t0 =
  foldl'
    (\t (k, v) -> T.replace ("{{" <> k <> "}}") v t)
    t0
    kvs

escapeHtml :: Text -> Text
escapeHtml =
  T.concatMap repl
  where
    repl '<'  = "&lt;"
    repl '>'  = "&gt;"
    repl '&'  = "&amp;"
    repl '"'  = "&quot;"
    repl c    = T.singleton c

-- サーバ側の boardNewerThan + BoardState (id DESC) とセットで差分配信
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
