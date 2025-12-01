{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Env
  ( Env(..)
  , AppM(..)
  , runAppM
  ) where

import App.Board (BoardState)
import Control.Concurrent.STM (TVar, TChan)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Text (Text)

data Env = Env
  { envDbPath     :: FilePath
  , envJwtSecret  :: Text
  , envBoardState :: TVar BoardState
  , envBoardChan  :: TChan ()
  }

newtype AppM a = AppM { unAppM :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runAppM :: Env -> AppM a -> IO a
runAppM env (AppM m) =
  runReaderT m env
