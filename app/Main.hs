module Main where

import           Control.Lens                       ((^.))
import           Control.Monad                      (return, void)
import           Data.Bool                          (Bool (..), not)
import           Data.Foldable                      (any)
import           Data.Foldable                      (Foldable)
import           Data.Function                      (($))
import           Data.Functor                       ((<$>))
import           Data.List                          (filter)
import           Data.Maybe                         (Maybe (..), maybe)
import           Data.Text                          (Text, isInfixOf, pack)
import           Prelude                            ()
import           System.IO                          (IO)
import           Web.Matrix.Bot.API                 (sendMessage)
import           Web.Matrix.Dirwatch.ConfigOptions  (coBotUrl, coDirectory,
                                                     coExclude, coRoomName,
                                                     readConfigOptions)
import           Web.Matrix.Dirwatch.Conversion     (convertDirwatchEvents)
import           Web.Matrix.Dirwatch.INotify        (Event, NotifyEvent (..),
                                                     eventFilePath,
                                                     makeRelative,
                                                     transformPath, unbuffer,
                                                     watchRecursiveBuffering)
import           Web.Matrix.Dirwatch.ProgramOptions (poConfigFile,
                                                     readProgramOptions)

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f x = not (any f x)

eventBlacklist :: [Text] -> NotifyEvent -> Bool
eventBlacklist blacklists (NotifyEvent _ e) =
  maybe True (\p -> none (`isInfixOf` pack p) blacklists) (eventFilePath e)

path :: NotifyEvent -> Event
path (NotifyEvent _ fn) = fn

main :: IO ()
main = do
  options <- readProgramOptions
  configOptions <- readConfigOptions (options ^. poConfigFile)
  watcher <- watchRecursiveBuffering (configOptions ^. coDirectory)
  unbuffer watcher $ \events ->
    case convertDirwatchEvents (transformPath (makeRelative (configOptions ^. coDirectory)) <$> (filter (eventBlacklist (configOptions ^. coExclude)) events)) of
      Nothing -> return ()
      Just message -> void $ sendMessage (configOptions ^. coBotUrl) (configOptions ^. coRoomName) message
