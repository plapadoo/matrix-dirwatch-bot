{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Maybe(Maybe(..))
import Control.Monad(return,void)
import Web.Matrix.Dirwatch.ProgramOptions(readProgramOptions,poBotUrl,poRoomName,poDirectory,poExclude)
import Web.Matrix.Dirwatch.INotify(watchRecursiveBuffering,unbuffer,transformPath,makeRelative,NotifyEvent(..))
import Web.Matrix.Dirwatch.Conversion(convertDirwatchEvents)
import Web.Matrix.Bot.IncomingMessage(IncomingMessage)
import Web.Matrix.Bot.API(sendMessage)
import Control.Lens((^.))
import Data.Functor((<$>))
import Prelude()
import System.IO(IO)
import qualified Data.Text as Text
import Data.Function((.),($))
import Data.Text(Text,pack,isInfixOf)
import Data.Bool(Bool(..),not)
import Data.Foldable(any)
import Data.List(filter)

none f x = not (any f x)

eventBlacklist :: [Text] -> NotifyEvent -> Bool
eventBlacklist blacklists (NotifyEvent path _) = none (`isInfixOf` pack path) blacklists

main :: IO ()
main = do
  options <- readProgramOptions
  watcher <- watchRecursiveBuffering (options ^. poDirectory)
  unbuffer watcher $ \events ->
    case convertDirwatchEvents (transformPath (makeRelative (options ^. poDirectory)) <$> (filter (eventBlacklist (options ^. poExclude)) events)) of
      Nothing -> return ()
      Just message -> void $ sendMessage (options ^. poBotUrl) (options ^. poRoomName) message
