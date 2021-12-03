{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates (
    Env (..),
    Template (..),
    loadEnv,
    generate,
) where

import Control.Monad (filterM, join, when, (<=<))
import Control.Monad.Extra (findM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as T
import Git.Libgit2 (LgRepo)
import Path (Abs, Dir, File, Path, Rel, dirname, filename, parseAbsDir, toFilePath, (</>))
import Path.IO (
    copyDirRecur,
    copyFile,
    doesDirExist,
    ensureDir,
    forgivingAbsence,
    ignoringAbsence,
    isSymlink,
    listDir,
 )
import System.Directory (
    canonicalizePath,
    createDirectoryLink,
    createFileLink,
    getSymbolicLinkTarget,
    pathIsSymbolicLink,
    removeFile,
 )
import qualified System.FilePath as FP
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import Text.Ginger.GVal (GVal)
import Text.Ginger.Html (Html, htmlSource)
import Text.Ginger.Parse (ParserError (..), SourcePos, parseGingerFile)
import Text.Ginger.Run (easyContext, runGingerT)

import Config (Config (..))
import Types

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
    , envOutputDirectory :: Path Abs Dir
    , envRepoPaths :: [Path Abs Dir]
    , envHost :: T.Text
    , envQuiet :: Bool
    , envForce :: Bool
    }

data Template = Template
    { templatePath :: Path Rel File
    , templateGinger :: G.Template SourcePos
    }

{-
This creates the runtime environment, collecting the config and loading template data
from the template directory.
-}
loadEnv :: Bool -> Bool -> Config -> IO Env
loadEnv quiet force config = do
    -- First ensure that the output directory exists
    output <- parseAbsDir <=< canonicalizePath . confOutputDirectory $ config
    ensureDir output

    -- Parse repos for env
    repoPaths <-
        if confScanRepoPaths config
            then do
                ps <- fmap concat . mapM (fmap fst . ls) . confRepoPaths $ config
                return . filter ((/=) ".git" . toFilePath . dirname) $ ps
            else mapM (parseAbsDir <=< canonicalizePath) . confRepoPaths $ config

    -- Find template files, copying the static files as is
    (dirs, files) <- ls . confTemplateDirectory $ config
    (_, filesRepo) <- ls $ confTemplateDirectory config FP.</> "repo"
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

    -- App environment
    return
        Env
            { envConfig = config
            , envIndexTemplates = indexT
            , envCommitTemplate = commitT
            , envFileTemplate = fileT
            , envRepoTemplates = repoT
            , envOutputDirectory = output
            , envRepoPaths = repoPaths
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
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate ::
    Path Abs File ->
    HashMap.HashMap T.Text (GVal RunRepo) ->
    Template ->
    ReaderT LgRepo IO ()
generate output context template = do
    let output' = toFilePath output
    liftIO . ignoringAbsence . removeFile $ output'
    content <- liftIO . newIORef . TB.fromText $ ""

    let emit :: Html -> ReaderT LgRepo IO ()
        emit = liftIO . modifyIORef' content . flip mappend . TB.fromText . htmlSource

    runGingerT (easyContext emit context) . templateGinger $ template
    result <- liftIO . readIORef $ content
    liftIO . T.writeFile output' . TB.toLazyText $ result

{-
This takes the session's `Config` and maybe returns a loaded template for the
``indexTemplate`` setting.
-}
loadTemplate :: Path Abs File -> IO (Maybe Template)
loadTemplate path =
    parseGingerFile includeResolver (toFilePath path) >>= \case
        Right parsed -> return . Just . Template (filename path) $ parsed
        Left err -> do
            informError (toFilePath path) err
            return Nothing
  where
    -- An attempt at pretty printing the error message.
    informError p (ParserError msg Nothing) =
        putStr $ "Template error: " <> p <> "\n" <> indent msg
    informError p (ParserError msg (Just pos)) =
        putStrLn $ "Template error: " <> p <> "\n" <> indent (show pos <> "\n" <> msg)
    indent = unlines . map (mappend "    ") . lines

    -- This resolves template 'includes'.
    includeResolver :: FilePath -> IO (Maybe String)
    includeResolver p = either (const Nothing) Just <$> tryIOError (readFile p)

{-
The logic for copying static files and folders. Any file or folder in the
``confTemplateDirectory`` is considered static if:

- it is a symbolic link, or
- it does not end in ".html" or ".include".

Symbolic links are not followed and are copied as is. This means that a symbolic link
from `confTemplateDirectory/link.html` to `gitserve/index.html` will be copied, keeping the
link intact, resulting in a symbolic link at `outputDirectory/link.html` essentially
pointing to `outputDirectory/gitserve/index.html`.
-}
-- TODO: merge isStatic and copy so it's just one step
copyStaticDirs :: Path Abs Dir -> [Path Abs Dir] -> IO ()
copyStaticDirs output = mapM_ copy <=< filterM isStatic
  where
    isStatic :: Path Abs Dir -> IO Bool
    isStatic p = do
        let isRepo = (toFilePath . dirname $ p) == "repo/"
        isLink <- pathIsSymbolicLink . toFilePath $ p
        return $ not isRepo || isLink
    copy :: Path Abs Dir -> IO ()
    copy p = do
        let output' = output </> dirname p
        let fp = FP.dropTrailingPathSeparator . toFilePath $ p
        isLink <- pathIsSymbolicLink fp
        if isLink
            then do
                target <- getSymbolicLinkTarget fp
                createDirectoryLink target . FP.dropTrailingPathSeparator . toFilePath $ output'
            else copyDirRecur p output'

copyStaticFiles :: Path Abs Dir -> [Path Abs File] -> IO ()
copyStaticFiles output = mapM_ copy <=< filterM isStatic
  where
    isStatic :: Path Abs File -> IO Bool
    isStatic p = do
        let fp = toFilePath p
        let isTemplate = FP.takeExtension fp `elem` [".html", ".include"]
        isLink <- pathIsSymbolicLink fp
        return $ not isTemplate || isLink
    copy :: Path Abs File -> IO ()
    copy p = do
        let fp = toFilePath p
        isLink <- pathIsSymbolicLink fp
        let output' = output </> filename p
        if isLink
            then do
                target <- getSymbolicLinkTarget fp
                maybeExists <- forgivingAbsence . isSymlink $ output'
                let exists = fromMaybe False maybeExists
                when exists . removeFile . toFilePath $ output'
                createFileLink target . toFilePath $ output'
            else copyFile p output'
