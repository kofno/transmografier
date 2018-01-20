{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Conversion (Conversion(..), Scale(..))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import FileOps
       (FileData(..), conversionPath, mkConversionDir, writeConversion,
        writePpt)
import Filesystem.Path.CurrentOS (FilePath, fromText)
import Network.HTTP.Types (status202, status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (FileInfo(..))
import Prelude hiding (FilePath)
import System.Environment (lookupEnv)
import Web.Scotty
       (ActionM, Parsable, ScottyM, files, get, json, middleware, param,
        post, rescue, scotty, status, text)

type Result a = Either Text a

app :: ScottyM ()
app = do
  middleware logStdoutDev
  get "/" $ text "hello"
  post "/api/convert" uploadConversion

lookupParam ::
     forall a. Parsable a
  => Text
  -> ActionM (Maybe a)
lookupParam p =
  let result = (Just <$> param p)
      fallback = const (return Nothing)
  in result `rescue` fallback

lookupOr ::
     forall a. Parsable a
  => a
  -> Text
  -> ActionM a
lookupOr orValue p = do
  mV <- lookupParam p
  return $ fromMaybe orValue mV

lookupScale :: ActionM Scale
lookupScale = do
  width <- lookupOr 408 "width"
  height <- lookupOr 704 "height"
  scale <- lookupParam "scale"
  let dimensions = Dimensions height width
  let scale' = Scale <$> scale
  return $ fromMaybe dimensions scale'

getConversion :: UUID -> Result FilePath -> ActionM (Result Conversion)
getConversion _ (Left err) = return $ Left err
getConversion key (Right file) = do
  context <- lookupOr "" "context"
  lossless <- lookupOr True "lossless"
  thumbs <- lookupOr True "thumbs"
  meta <- lookupOr True "meta"
  scale <- lookupScale
  return . Right $ Conversion {..}

getFileData :: ActionM (Result FileData)
getFileData = do
  fs <- files
  let upload' =
        [ FileData (fName fi) (contentType fi) (fileContent fi)
        | (field, fi) <- fs
        , field == "file"
        ]
  return $
    case upload' of
      [fi] -> Right fi
      _ -> Left "The `file` upload field is required"
  where
    contentType = decodeUtf8 . fileContentType
    fName = fromText . decodeUtf8 . fileName

uploadPpt :: UUID -> Result FileData -> IO (Result FilePath)
uploadPpt _ (Left err) = return $ Left err
uploadPpt key (Right fi) =
  let name' = return . Right . name
  in mkConversionDir "uploads" key >>= writePpt fi >> name' fi

storeConversion :: UUID -> Result Conversion -> IO (Result Conversion)
storeConversion _ conversion@(Left _) = return conversion
storeConversion key conversion@(Right c) = do
  writeConversion c $ conversionPath "uploads" key
  return conversion

uploadConversion :: ActionM ()
uploadConversion = do
  key <- liftIO nextRandom
  conversion <-
    getFileData >>= liftIO . uploadPpt key >>= getConversion key >>=
    liftIO . storeConversion key
  case conversion of
    Left err -> status status400 >> text err
    Right c -> status status202 >> json c

main :: IO ()
main = do
  port <- maybe 9000 read <$> lookupEnv "PORT"
  putStrLn $ "Running on port " ++ show port
  scotty port app