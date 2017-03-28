{-# LANGUAGE OverloadedStrings #-}

module GMB.Dirwatch.Conversion
  ( convertDirwatchEvents
  ) where

import Data.Foldable (length, fold)
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.List (take)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Ord ((>=))
import qualified Data.Text as Text
import Data.Text.Format (format)
import Data.Text.Format (format)
import Data.Text.Lazy (toStrict)
import GMB.Dirwatch.INotify (NotifyEvent(..), Event(..))
import GMB.IncomingMessage
       (IncomingMessage, constructIncomingMessageLazy,
        constructIncomingMessage)
import Plpd.Util (textShow, surroundHtml)
import Prelude ()
import System.FilePath ((</>))

convertEvent :: NotifyEvent -> Maybe Text.Text
convertEvent (NotifyEvent path event) =
  case event of
    Modified _ fp ->
      Just ("modified " <> Text.pack (maybe path (\p -> path </> p) fp))
    Attributes _ fp ->
      Just
        ("attributes changed " <> Text.pack (maybe path (\p -> path </> p) fp))
    MovedOut _ fp _ -> Just ("moved out " <> Text.pack (path </> fp))
    MovedIn _ fp _ -> Just ("moved in " <> Text.pack (path </> fp))
    MovedSelf fp -> Just ("moved self " <> Text.pack path)
    Created _ fp -> Just ("created " <> Text.pack (path </> fp))
    Deleted _ fp -> Just ("deleted " <> Text.pack (path </> fp))
    _ -> Nothing

convertDirwatchEvents :: [NotifyEvent] -> Maybe IncomingMessage
convertDirwatchEvents events =
  case catMaybes (convertEvent <$> events) of
    [] -> Nothing
    xs ->
      let lengthLimit = 20
          formatMessagesPlain = Text.intercalate ", "
          formatMessages xs =
            case xs of
              [x] -> x
              xs -> surroundHtml "ol" . surroundHtml "li" . Text.intercalate "</li><li>" $ xs
          messages = take lengthLimit xs
          wholeMessagePlain =
            if length xs >= lengthLimit
              then format
                     "{} change(s), showing first {}: {}"
                     (length xs, lengthLimit, formatMessagesPlain messages)
              else format
                     "{} change(s): {}"
                     (length xs, formatMessagesPlain messages)
          wholeMessage =
            if length xs >= lengthLimit
              then format
                     "{} change(s), showing first {}: {}"
                     (length xs, lengthLimit, formatMessages messages)
              else format
                     "{} change(s): {}"
                     (length xs, formatMessages messages)
      in Just
           (constructIncomingMessage
              (toStrict wholeMessagePlain)
              (Just (toStrict wholeMessage)))
