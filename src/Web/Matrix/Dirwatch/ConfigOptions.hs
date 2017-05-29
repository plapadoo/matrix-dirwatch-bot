{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Dirwatch.ConfigOptions
  ( ConfigOptions(..)
  , readConfigOptions
  , coBotUrl
  , coRoomName
  , coExclude
  , coDirectory
  ) where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Configurator (Worth(..), load, lookup, require)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe)
import qualified Data.Text as Text
import Prelude (error, undefined)
import System.FilePath(FilePath)

data ConfigOptions = ConfigOptions
  { _coBotUrl :: Text.Text
  , _coRoomName :: Text.Text
  , _coExclude :: [Text.Text]
  , _coDirectory :: FilePath
  }

makeLenses ''ConfigOptions

requireLift config key = liftIO (require config key)

readConfigOptions
  :: MonadIO m
  => FilePath -> m ConfigOptions
readConfigOptions configFilePath = do
  config <- liftIO (load [Required configFilePath])
  ConfigOptions <$> (requireLift config "bot-url") <*> (requireLift config "room-name") <*> (requireLift config "exclude") <*> (requireLift config "directory")
