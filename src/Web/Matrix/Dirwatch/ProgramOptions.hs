{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Dirwatch.ProgramOptions
  ( ProgramOptions(..)
  , readProgramOptions
  , poConfigFile
  ) where

import           Control.Applicative      ((<$>), (<*>))
import           Control.Lens             (makeLenses)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Monoid              ((<>))
import qualified Options.Applicative      as OptAppl
import           System.FilePath          (FilePath)

data ProgramOptions = ProgramOptions
  { _poConfigFile :: FilePath
  }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
  ProgramOptions <$>
    OptAppl.strOption
        (OptAppl.long "config-file" <>
         OptAppl.help "Where to put the config file" <>
         OptAppl.value "/etc/matrix-bot/matrix-dirwatch-bot.dhall")

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
