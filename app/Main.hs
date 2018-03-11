module Main where

import           Control.Applicative                (pure)
import           Control.Lens                       ((^.))
import           Data.Bool                          (Bool (..), not)
import           Data.Foldable                      (Foldable, any, foldMap)
import           Data.Function                      (id, ($), (.))
import           Data.Functor                       ((<$>))
import           Data.List                          (filter)
import           Data.Maybe                         (Maybe (..), maybe)
import           Data.Monoid                        ((<>))
import qualified Data.Text                          as Text
import qualified Data.Text.IO                       as TextIO
import qualified Data.Text.Lazy                     as LazyText
import           Lucid                              (body_, renderText)
import           Prelude                            ()
import           System.IO                          (IO)
import           Web.Matrix.Dirwatch.Conversion     (IncomingMessage (..),
                                                     convertDirwatchEvents)
import           Web.Matrix.Dirwatch.INotify        (Event, NotifyEvent (..),
                                                     eventFilePath,
                                                     makeRelative,
                                                     transformPath, unbuffer,
                                                     watchRecursiveBuffering)
import           Web.Matrix.Dirwatch.ProgramOptions (poDirectory, poExclusions,
                                                     readProgramOptions)

incomingMessageToText :: IncomingMessage -> Text.Text
incomingMessageToText (IncomingMessage plain markup) = (foldMap id (LazyText.toStrict . renderText . body_ <$> markup)) <> plain

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f x = not (any f x)

eventBlacklist :: [Text.Text] -> NotifyEvent -> Bool
eventBlacklist blacklists (NotifyEvent _ e) =
  maybe True (\p -> none (`Text.isInfixOf` Text.pack p) blacklists) (eventFilePath e)

path :: NotifyEvent -> Event
path (NotifyEvent _ fn) = fn

main :: IO ()
main = do
  options <- readProgramOptions
  watcher <- watchRecursiveBuffering (options ^. poDirectory)
  unbuffer watcher $ \events ->
    let exclusionTexts = Text.pack <$> (options ^. poExclusions)
        filtered = filter (eventBlacklist exclusionTexts) events
    in
      case convertDirwatchEvents (transformPath (makeRelative (options ^. poDirectory)) <$> filtered) of
        Nothing      -> pure ()
        Just message -> TextIO.putStrLn (incomingMessageToText message)
