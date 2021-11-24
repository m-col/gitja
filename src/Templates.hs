{-# LANGUAGE LambdaCase #-}

module Templates (
    -- The Env data type and its constructors.
    Env,
    envConfig,
    envTemplates,
    envIndexTemplate,
    envCommitTemplate,
    envFileTemplate,
    envForce,
    -- The Template data type and its constructors.
    Template,
    templatePath,
    templateGinger,
    -- The entrypoint used by main.
    loadTemplates,
    -- The core functionality for which templates are used.
    generate,
) where

import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HashMap
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Git.Libgit2 (LgRepo)
import Path (Abs, Dir, File, Path, dirname, filename, parseAbsDir, toFilePath, (</>))
import Path.IO (copyDirRecur, copyFile, ensureDir, listDir)
import System.Directory (makeAbsolute)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import Text.Ginger.GVal (GVal)
import Text.Ginger.Html (Html, htmlSource)
import Text.Ginger.Parse (SourcePos, parseGingerFile, peErrorMessage)
import Text.Ginger.Run (easyContext, runGingerT)

import Config
import Types

{-
The Env data type represents all of the program's state, including user configuration
and loaded template data. This can be accessed as immutable global state at any point.
-}
data Env = Env
    { envConfig :: Config
    , envTemplates :: [Template]
    , envIndexTemplate :: Maybe Template
    , envCommitTemplate :: Maybe Template
    , envFileTemplate :: Maybe Template
    , envForce :: Bool
    }

data Template = Template
    { templatePath :: FilePath
    , templateGinger :: G.Template SourcePos
    }

{-
This creates the runtime environment, collecting the config and loading template data
from file.
-}
loadTemplates :: Bool -> Config -> IO Env
loadTemplates force config = do
    -- Load files from template directory
    templateFiles <- getFiles config
    templates <- mapM loadTemplate (fmap toFilePath templateFiles)
    -- Scoped templates
    indexT <- loadTemplate . indexTemplate $ config
    commitT <- loadTemplate . commitTemplate $ config
    fileT <- loadTemplate . fileTemplate $ config
    -- Global environment
    return
        Env
            { envConfig = config
            , envTemplates = catMaybes templates
            , envIndexTemplate = indexT
            , envCommitTemplate = commitT
            , envFileTemplate = fileT
            , envForce = force
            }

{-
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate ::
    FilePath ->
    HashMap.HashMap Text (GVal RunRepo) ->
    Template ->
    ReaderT LgRepo IO ()
generate output context template = do
    liftIO $ writeFile output "" -- Clear contents of file if it exists
    void . runGingerT (easyContext (writeTo output) context) . templateGinger $ template

----------------------------------------------------------------------------------------
-- Private -----------------------------------------------------------------------------

{-
This takes the session's `Config` and maybe returns a loaded template for the
``indexTemplate`` setting.
-}
loadTemplate :: FilePath -> IO (Maybe Template)
loadTemplate path =
    parseGingerFile includeResolver path >>= \case
        Right parsed -> return . Just . Template path $ parsed
        Left err -> do
            putStrLn . peErrorMessage $ err
            return Nothing

{-
This resolves Ginger's template includes.
-}
includeResolver :: FilePath -> IO (Maybe String)
includeResolver path = either (const Nothing) Just <$> tryIOError (readFile path)

{-
getFiles will look inside the template directory and generate a list of paths to likely
valid template files and another for static files.

It's not very pretty, but we also copy the static files and folders from here because
I'm too lazy to work with Path too much.
-}
getFiles :: Config -> IO [Path Abs File]
getFiles config = do
    (dirs, files) <- ls . templateDirectory $ config
    let files' = filter (not . isSuffixOf "include" . toFilePath) files
    let templates = filter (isTemplate config) files'
    let statics = filter (`notElem` templates) files'
    output <- parseAbsDir <=< makeAbsolute . outputDirectory $ config
    ensureDir output
    copyStaticDirs output dirs
    copyStaticFiles output statics
    return templates
  where
    -- This gets fully qualified paths of the directory's contents
    ls :: FilePath -> IO ([Path Abs Dir], [Path Abs File])
    ls = listDir <=< parseAbsDir <=< makeAbsolute

{-
This is used to filter files in the template directory so that we only try to load HTML
files.
-}
isTemplate :: Config -> Path Abs File -> Bool
isTemplate config = isTemplate' . map toLower . toFilePath
  where
    isTemplate' :: FilePath -> Bool
    isTemplate' p = isSuffixOf "html" p && not (isTargeted config p)
    isTargeted :: Config -> FilePath -> Bool
    isTargeted c p = p `elem` [indexTemplate c, commitTemplate c, fileTemplate c]

{-
These copies static files/folders into the output folder unchanged.
-}
copyStaticFiles :: Path Abs Dir -> [Path Abs File] -> IO ()
copyStaticFiles output = mapM_ copy
  where
    copy :: Path Abs File -> IO ()
    copy path = copyFile path $ output </> filename path

copyStaticDirs :: Path Abs Dir -> [Path Abs Dir] -> IO ()
copyStaticDirs output = mapM_ copy
  where
    copy :: Path Abs Dir -> IO ()
    copy path = copyDirRecur path $ output </> dirname path

{-
This function gets the output HTML data and is responsible for saving it to file.
-}
writeTo :: FilePath -> Html -> ReaderT LgRepo IO ()
writeTo path = liftIO . appendFile path . unpack . htmlSource
