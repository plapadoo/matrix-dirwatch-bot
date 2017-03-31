{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Maybe(Maybe(..))
import Control.Monad(return,void)
import Web.Matrix.Dirwatch.ProgramOptions(readProgramOptions,poBotUrl,poRoomName,poDirectory)
import Web.Matrix.Dirwatch.INotify(watchRecursiveBuffering,unbuffer,transformPath,makeRelative)
import Web.Matrix.Dirwatch.Conversion(convertDirwatchEvents)
import Web.Matrix.Bot.IncomingMessage(IncomingMessage)
import Web.Matrix.Bot.API(sendMessage)
import Control.Lens((^.))
import Data.Functor((<$>))
import Prelude()
import System.IO(IO)
import qualified Data.Text as Text
import Data.Function(($))

main :: IO ()
main = do
  options <- readProgramOptions
  watcher <- watchRecursiveBuffering (options ^. poDirectory)
  unbuffer watcher $ \events ->
    case convertDirwatchEvents (transformPath (makeRelative (options ^. poDirectory)) <$> events) of
      Nothing -> return ()
      Just message -> void $ sendMessage (options ^. poBotUrl) (options ^. poRoomName) message
