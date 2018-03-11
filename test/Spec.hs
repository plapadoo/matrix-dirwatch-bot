{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens                   (to, (^.))
import           Control.Monad                  (return)
import           Data.Bool                      (Bool (..), not)
import           Data.Function                  ((.))
import           Data.Maybe                     (Maybe (..), fromJust)
import           Data.Text                      (Text, isInfixOf, pack)
import           Data.Text.IO                   (putStrLn)
import           Prelude                        ()
import           System.IO                      (IO)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.TH              (defaultMainGenerator)
import           Test.HUnit                     (assertBool)
import           Text.Show                      (Show, show)
import           Web.Matrix.Dirwatch.Conversion (IncomingMessage (..),
                                                 convertDirwatchEvents)
import           Web.Matrix.Dirwatch.INotify    (Event (..), NotifyEvent (..))

textShow :: Show a => a -> Text
textShow = pack . show

case_singleConversion :: IO ()
case_singleConversion =
  case convertDirwatchEvents [NotifyEvent "foo" (Modified False (Just "bar"))] of
    Nothing -> return ()
    Just result@(IncomingMessage plainBody markupBody) -> do
      putStrLn (textShow result)
      assertBool "doesn't contain \"modified\"" ("modified" `isInfixOf` plainBody)
      assertBool "contains list" (not ("<li>" `isInfixOf` (markupBody ^. to fromJust . to textShow)) )


case_listConversion :: IO ()
case_listConversion =
  case convertDirwatchEvents [NotifyEvent "foo" (Modified False (Just "bar")),NotifyEvent "bar" (Modified False (Just "baz"))] of
    Nothing -> return ()
    Just result@(IncomingMessage plainBody markupBody) -> do
      putStrLn (textShow result)
      assertBool "doesn't contain \"modified\"" ("modified" `isInfixOf` plainBody)
      assertBool "doesn't contain list" ("<li>" `isInfixOf` (markupBody ^. to fromJust . to textShow))

main :: IO ()
main = $(defaultMainGenerator)
