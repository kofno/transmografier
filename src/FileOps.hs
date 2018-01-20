{-# LANGUAGE OverloadedStrings #-}

module FileOps where

import Conversion (Conversion)
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Text (Text)
import Data.UUID (UUID, toText)
import Filesystem.Path (FilePath, (</>))
import Filesystem.Path.CurrentOS (encodeString, fromText)
import Prelude hiding (FilePath, writeFile)
import Turtle (mktree)

data FileData = FileData
  { name :: FilePath
  , contentType :: Text
  , content :: ByteString
  }

uuidToPath :: UUID -> FilePath
uuidToPath = fromText . toText

mkConversionDir :: FilePath -> UUID -> IO FilePath
mkConversionDir baseDir key = do
  mktree dir
  return dir
  where
    dir = baseDir </> uuidToPath key

writePpt :: FileData -> FilePath -> IO ()
writePpt fi dir = writeFile path $ content fi
  where
    path = encodeString (dir </> name fi)

writeConversion :: Conversion -> FilePath -> IO ()
writeConversion c f = writeFile path $ encode c
  where
    path = encodeString f

conversionPath :: FilePath -> UUID -> FilePath
conversionPath baseDir key =
  baseDir </> uuidToPath key </> fromText "conversion.json"
