{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Maybe(Maybe(..))
import Control.Monad(return,void)
import Web.Matrix.Dirwatch.ProgramOptions(readProgramOptions,poBotUrl,poRoomName,poDirectory)
import Web.Matrix.Dirwatch.INotify(watchRecursiveBuffering,unbuffer,)
import Web.Matrix.Dirwatch.Conversion(convertDirwatchEvents)
import Web.Matrix.Bot.IncomingMessage(IncomingMessage)
import Web.Matrix.Bot.API(sendMessage)
import Control.Lens((^.))
import Prelude()
import System.IO(IO)
import qualified Data.Text as Text
import Data.Function(($))

main :: IO ()
main = do
  options <- readProgramOptions
  watcher <- watchRecursiveBuffering (options ^. poDirectory)
  unbuffer watcher $ \events ->
    case convertDirwatchEvents events of
      Nothing -> return ()
      Just message -> void $ sendMessage (options ^. poBotUrl) (options ^. poRoomName) message
