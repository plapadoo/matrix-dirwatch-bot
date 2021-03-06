{-# LANGUAGE TemplateHaskell #-}

module Web.Matrix.Dirwatch.ProgramOptions
  ( ProgramOptions(..)
  , readProgramOptions
  , poDirectory
  , poExclusions
  ) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid            ((<>))
import           Data.String            (String)
import qualified Options.Applicative    as OptAppl
import           System.FilePath        (FilePath)

data ProgramOptions = ProgramOptions
  { _poDirectory  :: FilePath
  , _poExclusions :: [String]
  }

makeLenses ''ProgramOptions

programOptionsParser :: OptAppl.Parser ProgramOptions
programOptionsParser =
  ProgramOptions <$>
    OptAppl.strOption
        (OptAppl.long "directory" <>
         OptAppl.help "The directory to watch")
        <*>
        OptAppl.many (
            OptAppl.strOption
            (OptAppl.long "exclude" <>
            OptAppl.help "Exclude a directory"))

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
         OptAppl.header "matrix-dirwatch - send directory change events to stdout to be sent to matrix")
