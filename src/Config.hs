{-# LANGUAGE DeriveGeneric #-}  -- Required by Dhall
{-# LANGUAGE DerivingStrategies #-}

module Config (
    Config,
    repoPaths,
    templateDirectory,
    indexTemplate,
    getConfig,
    outputDirectory,
    host
) where

import Dhall

data Config = Config
    { repoPaths :: [FilePath]
    , templateDirectory :: FilePath
    , indexTemplate :: FilePath
    , outputDirectory :: FilePath
    , host :: Text
    }
    deriving stock (Generic, Show)

instance FromDhall Config

getConfig :: Text -> IO Config
getConfig = input auto
