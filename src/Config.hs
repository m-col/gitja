-- Required by Dhall
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config (
    Config (..),
    getConfig,
) where

import Control.Monad ((<=<))
import Data.Text (pack)
import Dhall
import System.Directory (makeAbsolute)

data Config = Config
    { repoPaths :: [FilePath]
    , templateDirectory :: FilePath
    , outputDirectory :: FilePath
    , host :: Text
    }
    deriving stock (Generic, Show)

instance FromDhall Config

getConfig :: String -> IO Config
getConfig = input auto . pack <=< makeAbsolute
