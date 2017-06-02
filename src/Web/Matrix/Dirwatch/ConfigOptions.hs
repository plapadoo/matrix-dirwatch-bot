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
import Data.Configurator (Worth(..), load, require)
import Data.Configurator.Types (Config,Name,Configured)
import Data.Functor ((<$>))
import qualified Data.Text as Text
import System.FilePath(FilePath)

data ConfigOptions = ConfigOptions
  { _coBotUrl :: Text.Text
  , _coRoomName :: Text.Text
  , _coExclude :: [Text.Text]
  , _coDirectory :: FilePath
  }

makeLenses ''ConfigOptions

requireLift :: (Configured a, MonadIO m) => Config -> Name -> m a
requireLift config key = liftIO (require config key)

readConfigOptions
  :: MonadIO m
  => FilePath -> m ConfigOptions
readConfigOptions configFilePath = do
  config <- liftIO (load [Required configFilePath])
  ConfigOptions <$> (requireLift config "bot-url") <*> (requireLift config "room-name") <*> (requireLift config "exclude") <*> (requireLift config "directory")
