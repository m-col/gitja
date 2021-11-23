{-# LANGUAGE OverloadedStrings #-}

module Main (
    main,
) where

import Paths_gitserve (version)

import Data.Version (showVersion)
import qualified Options.Applicative as O
import System.Directory (createDirectoryIfMissing)

import Config (getConfig, outputDirectory)
import Index (runIndex)
import Repositories (run)
import Templates (loadTemplates)

{-
Command line options
-}
data Options = Options
    { config :: FilePath
    , force :: Bool
    , printVersion :: Bool
    }

opts :: O.Parser Options
opts =
    Options
        <$> O.strOption
            ( O.long "config"
                <> O.short 'c'
                <> O.metavar "CONFIG"
                <> O.value "./config.dhall"
                <> O.help "Configuration file to use (Default: ./config.dhall)"
            )
        <*> O.switch
            ( O.long "force"
                <> O.short 'f'
                <> O.help "Force regeneration of all files"
            )
        <*> O.switch
            ( O.long "version"
                <> O.short 'v'
                <> O.help "Print the gitserve's version"
            )

parser :: IO Options
parser =
    O.execParser $
        O.info
            (opts O.<**> O.helper)
            ( O.progDesc "🐙 Templated web page generator for your git repositories"
            )

{-
Main logic
-}
main :: IO ()
main = do
    options <- parser
    if printVersion options
        then putStrLn $ "Your gitserve version is: " <> showVersion version
        else do
            conf <- getConfig (config options)
            createDirectoryIfMissing True $ outputDirectory conf
            env <- loadTemplates (force options) conf
            runIndex env =<< run env
