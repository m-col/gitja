{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Env (
    Config (..),
    getConfig,
    Env (..),
    loadEnv,
    generate,
) where

import Control.Monad (filterM, join, when, (<=<))
import Control.Monad.Extra (findM)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (pack)
import qualified Data.Text as T
import Dhall
import Dhall.Deriving
import Path (Abs, Dir, File, Path, dirname, filename, parseAbsDir, toFilePath, (</>))
import Path.IO (
    copyDirRecur,
    copyFile,
    doesDirExist,
    ensureDir,
    forgivingAbsence,
    isSymlink,
    listDir,
 )
import System.Directory (
    canonicalizePath,
    createDirectoryLink,
    createFileLink,
    getSymbolicLinkTarget,
    makeAbsolute,
    pathIsSymbolicLink,
    removeFile,
 )
import System.Exit (die)
import qualified System.FilePath as FP

import Templates (Template (..), generate, loadTemplate)

{-
The Config data type represents the configuration options available in the config file.
Each record of the type, without the 'conf' prefix, is an option.
-}
data Config = Config
    { confRepos :: [FilePath]
    , confScan :: Bool
    , confTemplate :: FilePath
    , confOutput :: FilePath
    , confHost :: Text
    }
    deriving stock (Generic)
    deriving
        (FromDhall)
        via Codec (Field (CamelCase <<< DropPrefix "conf")) Config

getConfig :: String -> IO Config
getConfig = input auto . pack <=< makeAbsolute

{-
The Env data type represents all of the program's state, including user configuration
and loaded template data. This can be accessed as immutable global state at any point.
-}
data Env = Env
    { envConfig :: Config
    , envIndexTemplates :: [Template]
    , envCommitTemplate :: Maybe Template
    , envFileTemplate :: Maybe Template
    , envRepoTemplates :: [Template]
    , envOutput :: Path Abs Dir
    , envRepos :: [Path Abs Dir]
    , envHost :: T.Text
    , envQuiet :: Bool
    , envForce :: Bool
    }

{-
This creates the runtime environment, collecting the config and loading template data
from the template directory.
-}
loadEnv :: Bool -> Bool -> Config -> IO Env
loadEnv quiet force config = do
    -- First ensure that the output directory exists
    output <- parseAbsDir <=< canonicalizePath . confOutput $ config
    ensureDir output

    -- Parse repos for env
    repos <-
        if confScan config
            then do
                ps <- fmap concat . mapM (fmap fst . ls) . confRepos $ config
                return . filter ((/=) ".git" . toFilePath . dirname) $ ps
            else mapM (parseAbsDir <=< canonicalizePath) . confRepos $ config

    -- Find template files, copying the static files as is
    (dirs, files) <- ls . confTemplate $ config
    (_, filesRepo) <- ls $ confTemplate config FP.</> "repo"
    copyStaticDirs output dirs
    copyStaticFiles output files

    -- Load files from template directory
    indexT <- collectTemplates files
    commitT <- findTemplate "commit.html" filesRepo
    fileT <- findTemplate "file.html" filesRepo
    repoT <-
        collectTemplates
            . filter (flip notElem ["commit.html", "file.html"] . toFilePath . filename)
            $ filesRepo

    -- Exit early if we didn't find any templates
    when
        ( null indexT
            && isNothing commitT
            && isNothing fileT
            && null repoT
        )
        $ die "No templates were found."

    -- App environment
    return
        Env
            { envConfig = config
            , envIndexTemplates = indexT
            , envCommitTemplate = commitT
            , envFileTemplate = fileT
            , envRepoTemplates = repoT
            , envOutput = output
            , envRepos = repos
            , envHost = confHost config
            , envQuiet = quiet
            , envForce = force
            }
  where
    ls :: FilePath -> IO ([Path Abs Dir], [Path Abs File])
    ls dir = do
        canon <- parseAbsDir =<< canonicalizePath dir
        exists <- doesDirExist canon
        if exists
            then listDir canon
            else return ([], [])

    collectTemplates :: [Path Abs File] -> IO [Template]
    collectTemplates = fmap catMaybes . mapM loadTemplate <=< filterM isMatch
      where
        isMatch p = do
            ((FP.takeExtension . toFilePath $ p) == ".html" &&)
                <$> (fmap not . pathIsSymbolicLink . toFilePath $ p)

    findTemplate :: FilePath -> [Path Abs File] -> IO (Maybe Template)
    findTemplate name = fmap join . mapM loadTemplate <=< findM isMatch
      where
        isMatch p =
            ((toFilePath . filename $ p) == name &&)
                <$> (fmap not . pathIsSymbolicLink . toFilePath $ p)

{-
The logic for copying static files and folders. Any file or folder in the
``confTemplate`` is considered static if:

- it is a symbolic link, or
- it does not end in ".html" or ".include".

Symbolic links are not followed and are copied as is. This means that a symbolic link
from `confTemplate/link.html` to `gitserve/index.html` will be copied, keeping the
link intact, resulting in a symbolic link at `output/link.html` essentially
pointing to `output/gitserve/index.html`.
-}
copyStaticDirs :: Path Abs Dir -> [Path Abs Dir] -> IO ()
copyStaticDirs output = mapM_ copy
  where
    copy :: Path Abs Dir -> IO ()
    copy p = do
        let isRepo = (toFilePath . dirname $ p) == "repo/"
        isLink <- pathIsSymbolicLink . toFilePath $ p
        when (not isRepo || isLink) $ do
            let output' = output </> dirname p
            let fp = FP.dropTrailingPathSeparator . toFilePath $ p
            if isLink
                then do
                    target <- getSymbolicLinkTarget fp
                    createDirectoryLink target . FP.dropTrailingPathSeparator . toFilePath $ output'
                else copyDirRecur p output'

copyStaticFiles :: Path Abs Dir -> [Path Abs File] -> IO ()
copyStaticFiles output = mapM_ copy
  where
    copy :: Path Abs File -> IO ()
    copy p = do
        let fp = toFilePath p
        let isTemplate = FP.takeExtension fp `elem` [".html", ".include"]
        isLink <- pathIsSymbolicLink fp
        when (not isTemplate || isLink) $ do
            let output' = output </> filename p
            if isLink
                then do
                    target <- getSymbolicLinkTarget fp
                    maybeExists <- forgivingAbsence . isSymlink $ output'
                    let exists = fromMaybe False maybeExists
                    when exists . removeFile . toFilePath $ output'
                    createFileLink target . toFilePath $ output'
                else copyFile p output'
