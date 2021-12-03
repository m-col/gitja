{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
    main,
) where

import Paths_gitserve (version)

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Data.FileEmbed (embedDir, embedFile)
import Data.Version (showVersion)
import qualified Options.Applicative as O
import qualified System.Directory as D
import System.FilePath (takeDirectory, (</>))

import Config (getConfig)
import Index (runIndex)
import Repositories (run)
import Templates (loadEnv)

{-
Command line options
-}
data Options = Options
    { optConfig :: FilePath
    , optQuiet :: Bool
    , optForce :: Bool
    , optTemplate :: Bool
    , optVersion :: Bool
    }

opts :: O.Parser Options
opts =
    Options
        <$> O.strOption
            ( O.long "config"
                <> O.short 'c'
                <> O.metavar "CONFIG"
                <> O.value "./config.dhall"
                <> O.help "Configuration file to use (Default: ./config.dhall)."
            )
        <*> O.switch
            ( O.long "quiet"
                <> O.short 'q'
                <> O.help "Suppress non-error output."
            )
        <*> O.switch
            ( O.long "force"
                <> O.short 'f'
                <> O.help "Force regeneration of all files."
            )
        <*> O.switch
            ( O.long "template"
                <> O.short 't'
                <> O.help "Create a template and config in the current folder."
            )
        <*> O.switch
            ( O.long "version"
                <> O.short 'v'
                <> O.help "Print the gitserve's version."
            )

parser :: IO Options
parser =
    O.execParser $
        O.info
            (opts O.<**> O.helper)
            ( O.progDesc . B.toString $ $(embedFile "description")
            )

{-
Main logic
-}
main :: IO ()
main = do
    options <- parser
    if
            | optVersion options ->
                putStrLn $ "Your gitserve version is: " <> showVersion version
            | optTemplate options ->
                makeTemplate
            | otherwise -> do
                conf <- getConfig (optConfig options)
                env <- loadEnv (optQuiet options) (optForce options) conf
                runIndex env =<< run env

{-
Put a base template and plain config into the current directory.
-}
makeTemplate :: IO ()
makeTemplate = do
    tExists <- D.doesPathExist "./template"
    cExists <- D.doesPathExist "./config.dhall"
    if
            | tExists -> putStrLn "Failed - ./template already exists."
            | cExists -> putStrLn "Failed - ./config.dhall already exists."
            | otherwise -> do
                mapM_ (uncurry place) base
                B.writeFile "./config.dhall" config
                putStrLn "A base template as been put at ./template."
                putStrLn "A plain config has been put at ./config.dhall"
                putStrLn "Add a local git repository to 'repos' in the config"
                putStrLn "and run gitserve to generate HTML in ./output."
                oExists <- D.doesPathExist "./output"
                when oExists . putStrLn $
                    "WARNING: ./output ALREADY EXISTS AND WILL BE OVERWRITTEN\n"
                        <> "UNLESS YOU MOVE/RENAME IT OR CHANGE GITSERVE'S output."
  where
    base :: [(FilePath, B.ByteString)]
    base = $(embedDir "templates/base")

    config :: B.ByteString
    config = $(embedFile "src/config.dhall")

    place :: FilePath -> B.ByteString -> IO ()
    place path bytes = do
        let path' = "template" </> path
        D.createDirectoryIfMissing True . takeDirectory $ path'
        B.writeFile path' bytes
