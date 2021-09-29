{-# Language LambdaCase #-}

module Templates (
    -- The Template data type and its constructors.
    Template,
    templatePath,
    templateGinger,
    -- The Env data type and its constructors.
    Env,
    envConfig,
    envTemplates,
    envIndexTemplate,
    envCommitTemplate,
    envFileTemplate,
    -- The entrypoint used by main.
    loadTemplates,
    -- The core functionality for which templates are used.
    generate,
) where

import Control.Monad (filterM, (<=<))
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isSuffixOf)
import Data.Text (unpack, Text)
import qualified Data.HashMap.Strict as HashMap
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import Text.Ginger.Parse (SourcePos, parseGingerFile, peErrorMessage)
import Text.Ginger.GVal (GVal)
import Text.Ginger.Html (htmlSource, Html)
import Text.Ginger.Run (easyRenderM, Run, RuntimeError)

import Config

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
    }

data Template = Template
    { templatePath :: FilePath
    , templateGinger :: G.Template SourcePos
    }

{-
This creates the runtime environment, collecting the config and loading template data
from file.
-}
loadTemplates :: Config -> IO Env
loadTemplates config = do
    -- Custom templates
    files <- getFiles config
    parsed <- sequence $ parseGingerFile includeResolver <$> files
    let templates = zipWith ($) (Template <$> files) (rights parsed)  -- TODO: lefts not filtered from files
    -- Scoped templates
    indexT <- loadTemplate $ indexTemplate config
    commitT <- loadTemplate $ commitTemplate config
    fileT <- loadTemplate $ fileTemplate config
    -- Global environment
    return Env { envConfig = config
    , envTemplates = templates
    , envIndexTemplate = indexT
    , envCommitTemplate = commitT
    , envFileTemplate = fileT
    }

{-
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate
    :: FilePath
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
    -> Template
    -> IO (Either (RuntimeError SourcePos) (GVal (Run SourcePos IO Html)))
generate output context template = do
    let target = (</>) output . takeFileName . templatePath $ template
    writeFile target ""  -- Clear contents of file if it exists
    easyRenderM (writeTo target) context . templateGinger $ template

----------------------------------------------------------------------------------------
-- Private -----------------------------------------------------------------------------

{-
This takes the session's `Config` and maybe returns a loaded template for the
``indexTemplate`` setting.
-}
loadTemplate :: FilePath -> IO (Maybe Template)
loadTemplate path = parseGingerFile includeResolver path >>= \case
    Right parsed -> return . Just . Template path $ parsed
    Left err -> do
        print . peErrorMessage $ err
        return Nothing

{-
This is a Ginger `IncludeResolver` that will eventually be extended to enable caching of
includes.
-}
includeResolver :: FilePath -> IO (Maybe String)
includeResolver path = either (const Nothing) Just <$> tryIOError (readFile path)

{-
This is used to filter files in the template directory so that we only try to load
HTML/CSS/JS files.
-}
isTemplate :: FilePath -> IO Bool
isTemplate path = (&&) (isTemplate' (map toLower path)) <$> doesFileExist path
  where
    isTemplate' :: FilePath -> Bool
    isTemplate' p = isSuffixOf "html" p || isSuffixOf "css" p || isSuffixOf "js" p

{-
This is used to filter files in the template directory to exclude those specified by the
config settings ``indexTemplate``, ``commitTemplate`` or ``fileTemplate``.
-}
isScoped :: Config -> FilePath -> Bool
isScoped config path =
    path /= indexTemplate config &&
    path /= commitTemplate config &&
    path /= fileTemplate config

{-
This wraps getDirectoryContents so that we get a list of fully qualified paths of the
directory's contents.
-}
listTemplates :: FilePath -> IO [FilePath]
listTemplates directory = fmap (directory </>) <$> getDirectoryContents directory

{-
getFiles will look inside the template directory and generate a list of paths to likely
valid template files.
-}
getFiles :: Config -> IO [FilePath]
getFiles config =
    filter (isScoped config) <$> (filterM isTemplate <=< listTemplates . templateDirectory $ config)

{-
This function gets the output HTML data and is responsible for saving it to file.
-}
writeTo :: FilePath -> Html -> IO ()
writeTo path = appendFile path . unpack . htmlSource
