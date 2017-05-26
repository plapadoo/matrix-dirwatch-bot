{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Dirwatch.ProgramOptions
  ( ProgramOptions(..)
  , readProgramOptions
  , poBotUrl
  , poRoomName
  , poDirectory
  , poExclude
  ) where

import Control.Applicative ((<$>), (<*>),many)
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Options.Applicative as OptAppl
import Options.Applicative.Text (textOption)
import Prelude (Int)
import System.FilePath (FilePath)
import System.IO (IO)

data ProgramOptions = ProgramOptions
  { _poBotUrl :: Text
  , _poRoomName :: Text
  , _poExclude :: [Text]
  , _poDirectory :: FilePath
  }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
  ProgramOptions <$>
  textOption (OptAppl.long "bot-url" <> OptAppl.help "matrix bot URL") <*>
  textOption (OptAppl.long "room-name" <> OptAppl.help "internal matrix room ID") <*>
  many ( textOption (OptAppl.long "exclude" <> OptAppl.help "exclusion string") ) <*>
  OptAppl.strOption (OptAppl.long "directory" <> OptAppl.help "directory to watch")

readProgramOptions
  :: MonadIO m
  => m ProgramOptions
readProgramOptions = liftIO (OptAppl.execParser opts)
  where
    opts =
      OptAppl.info
        (OptAppl.helper <*> programOptionsParser)
        (OptAppl.fullDesc <>
         OptAppl.progDesc
           "Listen for directory changes" <>
         OptAppl.header "matrix-dirwatch - send directory change events to matrix")
