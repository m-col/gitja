{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates (
    Env (..),
    Template (..),
    loadEnv,
    generate,
) where

import Control.Monad (join, void, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Git.Libgit2 (LgRepo)
import Path (Abs, Dir, File, Path, Rel, dirname, filename, parseAbsDir, toFilePath, (</>))
import Path.IO (copyDirRecur, copyFile, doesDirExist, ensureDir, listDir)
import System.Directory (canonicalizePath)
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
    , envHost :: Text
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
loadEnv :: Bool -> Config -> IO Env
loadEnv force config = do
    -- First ensure that the output directory exists
    output <- parseAbsDir <=< canonicalizePath . outputDirectory $ config
    ensureDir output

    -- Parse repos for env
    repoPaths' <- mapM (parseAbsDir <=< canonicalizePath) . repoPaths $ config

    -- Find template files, copying the static files as is
    (dirs, files) <- ls . templateDirectory $ config
    (_, filesRepo) <- ls $ templateDirectory config FP.</> "repo"
    copyStaticDirs output dirs
    copyStaticFiles output files

    -- Load files from template directory
    indexT <- collectTemplates files
    commitT <- fmap join . mapM loadTemplate . find ((==) "commit.html" . toFilePath . filename) $ filesRepo
    fileT <- fmap join . mapM loadTemplate . find ((==) "file.html" . toFilePath . filename) $ filesRepo
    repoT <- collectTemplates . filter (flip notElem ["commit.html", "file.html"] . toFilePath . filename) $ filesRepo

    -- Global environment
    return
        Env
            { envConfig = config
            , envIndexTemplates = indexT
            , envCommitTemplate = commitT
            , envFileTemplate = fileT
            , envRepoTemplates = repoT
            , envOutputDirectory = output
            , envRepoPaths = repoPaths'
            , envHost = host config
            , envForce = force
            }
  where
    collectTemplates :: [Path Abs File] -> IO [Template]
    collectTemplates = fmap catMaybes . mapM loadTemplate . filter ((==) ".html" . FP.takeExtension . toFilePath)

    ls :: FilePath -> IO ([Path Abs Dir], [Path Abs File])
    ls dir = do
        canon <- parseAbsDir =<< canonicalizePath dir
        exists <- doesDirExist canon
        if exists
            then listDir canon
            else return ([], [])

    copyStaticDirs :: Path Abs Dir -> [Path Abs Dir] -> IO ()
    copyStaticDirs output = mapM_ (\p -> copyDirRecur p (output </> dirname p)) . filterStaticDirs
    filterStaticDirs = filter ((/=) "repo" . toFilePath)

    copyStaticFiles :: Path Abs Dir -> [Path Abs File] -> IO ()
    copyStaticFiles output = mapM_ (\p -> copyFile p (output </> filename p)) . filterStaticFiles
    filterStaticFiles = filter (flip notElem [".html", ".include"] . FP.takeExtension . toFilePath)

{-
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate ::
    Path Abs File ->
    HashMap.HashMap Text (GVal RunRepo) ->
    Template ->
    ReaderT LgRepo IO ()
generate output context template = do
    let output' = toFilePath output
    liftIO $ writeFile output' "" -- Clear contents of file if it exists
    void . runGingerT (easyContext (writeTo output') context) . templateGinger $ template
  where
    -- This function gets the output HTML data and is responsible for saving it to file.
    writeTo :: FilePath -> Html -> ReaderT LgRepo IO ()
    writeTo path = liftIO . appendFile path . unpack . htmlSource

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
