{-# LANGUAGE DeriveGeneric #-}

module Config (
    Config,
    repoPaths,
    templateDirectory,
    outputDirectory,
    getConfig
) where

import Dhall

data Config = Config
    { repoPaths :: [FilePath]
    , templateDirectory :: FilePath
    , outputDirectory :: FilePath
    }
    deriving (Generic, Show)

instance FromDhall Config

getConfig :: Text -> IO Config
getConfig = input auto
