{-# Language LambdaCase #-}

module Templates (
    Template,
    templateGinger,
    loadTemplates,
    generate,
) where

import Control.Monad (filterM, (<=<))
import Data.Char (toLower)
import Data.Either (rights, Either)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Text (unpack, Text)
import qualified Data.HashMap.Strict as HashMap
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), FilePath)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import qualified Text.Ginger.Parse as G
import Text.Ginger.GVal (toGVal, GVal, ToGVal)
import Text.Ginger.Html (htmlSource, Html)
import Text.Ginger.Run (easyRenderM, Run, RuntimeError)

import Config (Config, templateDirectory)

data Template = Template
    { templatePath :: FilePath
    , templateGinger :: G.Template G.SourcePos
    }

{-
This is the main function of this module, which takes the session's `Config` and returns
the set of available templates loaded from files found in the template directory.
-}
loadTemplates :: Config -> IO [Template]
loadTemplates config = do
    files <- getFiles config
    parsed <- sequence $ G.parseGingerFile includeResolver <$> files 
    return $ Template <$> files <*> rights parsed

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
isTemplate path = ((&&) $ isTemplate' path) <$> (doesFileExist path)
  where
    p = map toLower path
    isTemplate' :: FilePath -> Bool
    isTemplate' path = isSuffixOf "html" p || isSuffixOf "css" p || isSuffixOf "js" p

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
generate :: HashMap.HashMap Text Text -> Template ->
    IO (Either (RuntimeError G.SourcePos) (GVal (Run G.SourcePos IO Html)))
generate context template = easyRenderM writeTo context (templateGinger template)
