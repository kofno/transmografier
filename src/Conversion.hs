{-# LANGUAGE DeriveGeneric #-}

module Conversion
  ( Conversion(..)
  , Scale(..)
  ) where

import Data.Aeson
       (FromJSON(..), ToJSON(..), defaultOptions, genericToEncoding,
        withText)
import Data.Either (either)
import qualified Data.Text.Lazy as L
import Data.UUID (UUID)
import Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FilePath
import GHC.Generics
import Prelude hiding (FilePath)

data Conversion = Conversion
  { key :: UUID
  , context :: L.Text
  , lossless :: Bool
  , thumbs :: Bool
  , meta :: Bool
  , scale :: Scale
  , file :: FilePath
  } deriving (Generic)

instance ToJSON Conversion where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Conversion

data Scale
  = Scale { percentage :: Int }
  | Dimensions { height :: Int
               , width :: Int }
  deriving (Show, Eq, Generic)

instance ToJSON Scale where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Scale

instance ToJSON FilePath where
  toJSON = toJSON . either id id . FilePath.toText

instance FromJSON FilePath where
  parseJSON = withText "FilePath" $ pure . FilePath.fromText
