{-# LANGUAGE OverloadedStrings #-}

module App.Board
  ( BoardState
  , emptyBoard
  , fromPosts
  , boardAllDesc
  , boardInsert
  , boardNewerThan
  ) where

import App.Types
  ( Post(..)
  , PostId(..)
  , unPostId
  )
import Data.List (sortOn)
import Data.Ord (Down(..))

newtype BoardState = BoardState { bsPosts :: [Post] }
  deriving (Eq, Show)

emptyBoard :: BoardState
emptyBoard = BoardState []

fromPosts :: [Post] -> BoardState
fromPosts ps =
  let sorted = sortOn (Down . unPostId . postId) ps
   in BoardState (take 1000 sorted)

boardAllDesc :: BoardState -> [Post]
boardAllDesc (BoardState ps) = ps

boardInsert :: Post -> BoardState -> BoardState
boardInsert p (BoardState ps) =
  BoardState (take 1000 (p : ps))

boardNewerThan :: PostId -> BoardState -> [Post]
boardNewerThan (PostId after) (BoardState ps) =
  takeWhile (\p -> unPostId (postId p) > after) ps
