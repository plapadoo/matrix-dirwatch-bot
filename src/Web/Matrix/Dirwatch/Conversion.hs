{-# LANGUAGE OverloadedStrings #-}

module Web.Matrix.Dirwatch.Conversion
  ( convertDirwatchEvents
  ) where

import Data.Foldable (length, fold)
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.List (take)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Control.Monad(mapM_)
import Data.Ord ((>=))
import qualified Data.Text as Text
import Data.Text.Format (format,Only(..))
import Data.Text.Lazy (toStrict)
import Web.Matrix.Dirwatch.INotify (NotifyEvent(..), Event(..))
import Web.Matrix.Bot.IncomingMessage
       (IncomingMessage, constructIncomingMessageLazy,
        constructIncomingMessage)
import Plpd.Util (textShow, surroundHtml)
import Prelude ()
import System.FilePath ((</>))
import Lucid(Html,toHtml,li_,ol_,div_)

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

convertDirwatchEvents :: [NotifyEvent] -> Maybe (IncomingMessage Text.Text (Html ()))
convertDirwatchEvents events =
  case catMaybes (convertEvent <$> events) of
    [] -> Nothing
    xs ->
      let lengthLimit = 20
          formatMessagesPlain = Text.intercalate ", "
          formatMessages xs =
            case xs of
              [x] -> toHtml x
              xs -> ol_ ( mapM_ (li_ . toHtml) xs )
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
            then toHtml (format "{} change(s), showing first {}: " (length xs, lengthLimit)) <> formatMessages xs
            else toHtml (format "{} change(s): " (Only (length xs))) <> formatMessages xs
      in Just
           (constructIncomingMessage
              (toStrict wholeMessagePlain)
              (Just wholeMessage))
