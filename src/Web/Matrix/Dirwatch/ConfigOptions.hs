{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Matrix.Dirwatch.ConfigOptions
  ( ConfigOptions(..)
  , readConfigOptions
  , coBotUrl
  , coRoomName
  , coExclude
  , coDirectory
  ) where

import           Control.Lens    (Getter, to)
import           Data.Foldable   (toList)
import           Data.Function   ((.))
import           Data.Functor    ((<$>))
import           Data.String     (String, fromString)
import qualified Data.Text       as Text
import qualified Dhall           as Dhall
import           GHC.Generics    (Generic)
import           Plpd.Dhall      (toString, toText)
import           System.FilePath (FilePath)
import           System.IO       (IO)

data ConfigOptions = ConfigOptions
    { botUrl    :: Dhall.Text
    , roomName  :: Dhall.Text
    , exclude   :: Dhall.Vector Dhall.Text
    , directory :: Dhall.Text
    } deriving(Generic,Dhall.Interpret)

readConfigOptions :: String -> IO ConfigOptions
readConfigOptions = Dhall.detailed . Dhall.input Dhall.auto . fromString

coDirectory :: Getter ConfigOptions FilePath
coDirectory = to (toString . directory)

coExclude :: Getter ConfigOptions ([Text.Text])
coExclude = to (\x -> toList (((toText <$>) . exclude) x))

coRoomName :: Getter ConfigOptions Text.Text
coRoomName = to (toText . roomName)

coBotUrl :: Getter ConfigOptions Text.Text
coBotUrl = to (toText . botUrl)
