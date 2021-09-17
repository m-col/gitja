{-# LANGUAGE DeriveGeneric #-}

module Config (
    Config,
    repoPaths,
    getConfig
) where

import Dhall

data Config = Config
    { repoPaths :: [FilePath]
    , outputDirectory :: FilePath
    }
    deriving (Generic, Show)

instance FromDhall Config

getConfig :: Text -> IO Config
getConfig = input auto
