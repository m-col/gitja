{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Data.Text (Text)
import qualified Options.Applicative as O

import Config (getConfig)
import Index (runIndex)
import Templates (loadTemplates)
import Repositories (run)

{-
Command line options
-}
data Options = Options
    { config :: Text
    , force :: Bool
    , version :: Bool
    }

opts :: O.Parser Options
opts = Options
    <$> O.strOption (
        O.long "config" <>
        O.short 'c' <>
        O.metavar "CONFIG" <>
        O.value "./config.dhall" <>
        O.help "Configuration file to use (Default: ./config.dhall)"
        )
    <*> O.switch (
        O.long "force" <>
        O.short 'f' <>
        O.help "Force regeneration of all files"
        )
    <*> O.switch (
        O.long "version" <>
        O.short 'v' <>
        O.help "Print the gitserve's version"
        )

parser :: IO Options
parser = O.execParser $ O.info (opts O.<**> O.helper)
    ( O.progDesc "🐙 Templated web page generator for your git repositories"
    )

{-
Main logic
-}
main :: IO ()
main = do
    options <- parser
    if version options
        then
            putStrLn $ "Your gitserve version is: " <> currentVersion
        else do
            conf <- getConfig (config options)
            env <- loadTemplates (force options) conf
            repos <- runIndex env
            run env repos

currentVersion :: String
currentVersion = "0.0.0"
