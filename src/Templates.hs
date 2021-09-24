{-# Language LambdaCase #-}

module Templates (
    Template,
    templateGinger,
    loadTemplates,
    loadIndexTemplate,
    generate,
) where

import Control.Monad (filterM, (<=<))
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isSuffixOf)
import Data.Text (unpack, Text)
import qualified Data.HashMap.Strict as HashMap
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import Text.Ginger.Parse (SourcePos, parseGingerFile)
import Text.Ginger.GVal (GVal)
import Text.Ginger.Html (htmlSource, Html)
import Text.Ginger.Run (easyRenderM, Run, RuntimeError)

import Config (Config, templateDirectory, indexTemplate)

data Template = Template
    { templatePath :: FilePath
    , templateGinger :: G.Template SourcePos
    }

{-
This takes the session's `Config` and returns the set of available templates loaded from
files found in the template directory, excluding the index template if it is in the same
directory.
-}
loadTemplates :: Config -> IO [Template]
loadTemplates config = do
    files <- getFiles config
    parsed <- sequence $ parseGingerFile includeResolver <$> files
    return $ Template <$> files <*> rights parsed

{-
This takes the session's `Config` and maybe returns a loaded template for the
``indexTemplate`` setting.
-}
loadIndexTemplate :: Config -> IO (Maybe Template)
loadIndexTemplate config = parseGingerFile includeResolver file >>= \case
    Right parsed -> return . Just . Template file $ parsed
    Left err -> do
        print err
        return Nothing
  where
    file = indexTemplate config

----------------------------------------------------------------------------------------

{-
This is a Ginger `IncludeResolver` that will eventually be extended to enable caching of
includes.
-}
includeResolver :: FilePath -> IO (Maybe String)
includeResolver path = tryIOError (readFile path) >>= \case
    Right contents -> return $ Just contents
    Left err -> do
        print err
        return Nothing

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
getFiles = filterM isTemplate <=< listTemplates . templateDirectory

{-
This function gets the output HTML data and is responsible for saving it to file.
-}
writeTo :: Html -> IO ()
writeTo = putStr . unpack . htmlSource

{-
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate
    :: HashMap.HashMap Text (GVal (Run SourcePos IO Html))
    -> Template
    -> IO (Either (RuntimeError SourcePos) (GVal (Run SourcePos IO Html)))
generate context template = easyRenderM writeTo context (templateGinger template)
