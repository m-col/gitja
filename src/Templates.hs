module Templates (
    Template,
    templateGinger,
    templatePath,
    loadTemplates,
) where

import Control.Monad (filterM, (<=<))
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), FilePath)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import qualified Text.Ginger.Parse as G

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
includeResolver path = tryIOError (readFile path) >>= \e ->
    case e of
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
