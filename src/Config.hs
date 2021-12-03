-- Required by Dhall
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Config (
    Config (..),
    getConfig,
) where

import Control.Monad ((<=<))
import Data.Text (pack)
import Dhall
import Dhall.Deriving
import System.Directory (makeAbsolute)

data Config = Config
    { confRepoPaths :: [FilePath]
    , confScanRepoPaths :: Bool
    , confTemplateDirectory :: FilePath
    , confOutputDirectory :: FilePath
    , confHost :: Text
    }
    deriving stock (Generic)
    deriving
        (FromDhall)
        via Codec (Field (CamelCase <<< DropPrefix "conf")) Config

getConfig :: String -> IO Config
getConfig = input auto . pack <=< makeAbsolute
