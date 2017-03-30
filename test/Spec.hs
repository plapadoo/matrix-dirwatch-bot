{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.TH (defaultMainGenerator)
import Test.HUnit(assertFailure,assertBool,assertEqual)
import Web.Matrix.Dirwatch.Conversion(convertDirwatchEvents)
import Web.Matrix.Dirwatch.INotify(NotifyEvent(..),Event(..))
import Data.Bool(Bool(..),not)
import Web.Matrix.Bot.IncomingMessage(plainBody,markupBody)
import Prelude()
import System.IO(IO)
import Data.Functor((<$>))
import Control.Monad(return)
import Data.Maybe(Maybe(..),fromJust)
import Data.Text.IO(putStrLn)
import Data.Text(isInfixOf)
import Control.Lens((^.),to)
import Data.Either(Either(..))
import Data.Function(($),(.))
import Data.Monoid((<>))
import Plpd.Util(textShow)

case_singleConversion =
  case convertDirwatchEvents [NotifyEvent "foo" (Modified False (Just "bar"))] of
    Nothing -> return ()
    Just result -> do
      putStrLn (textShow result)
      assertBool "doesn't contain \"modified\"" ("modified" `isInfixOf` (result ^. plainBody))
      assertBool "contains list" (not ("<li>" `isInfixOf` (result ^. markupBody . to fromJust . to textShow)) )


case_listConversion =
  case convertDirwatchEvents [NotifyEvent "foo" (Modified False (Just "bar")),NotifyEvent "bar" (Modified False (Just "baz"))] of
    Nothing -> return ()
    Just result -> do
      putStrLn (textShow result)
      assertBool "doesn't contain \"modified\"" ("modified" `isInfixOf` (result ^. plainBody))
      assertBool "doesn't contain list" ("<li>" `isInfixOf` (result ^. markupBody . to fromJust . to textShow))

main :: IO ()
main = $(defaultMainGenerator)
