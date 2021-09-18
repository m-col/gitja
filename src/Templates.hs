module Templates (
    Template,
    templatePath,
    templateContents,
    loadTemplates,
) where

import Control.Monad (filterM, (<=<))
import Data.Char (toLower)
import Data.List (isSuffixOf)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), FilePath)
import System.IO.Error (tryIOError)

import Config (Config, templateDirectory)

data Template = Template
    { templatePath :: FilePath
    , templateContents :: String
    }

--tryLoad :: FilePath -> IO (Maybe String)
--tryLoad path = do
--    e <- tryIOError $ readFile path
--    case e of
--        Right contents ->
--            return $ Just contents
--        Left error -> do
--            print error
--            return Nothing

loadTemplate :: FilePath -> IO Template
loadTemplate path = do
    contents <- readFile path
    return $ Template path contents

isTemplate :: FilePath -> IO Bool
isTemplate path = ((&&) $ isTemplate' path) <$> (doesFileExist path)

isTemplate' :: FilePath -> Bool
isTemplate' path = isSuffixOf "html" p || isSuffixOf "css" p || isSuffixOf "js" p
  where
    p = map toLower path

listTemplates :: FilePath -> IO [FilePath]
listTemplates directory = fmap (directory </>) <$> getDirectoryContents directory

loadTemplates :: Config -> IO [FilePath]
loadTemplates = filterM isTemplate <=< listTemplates . templateDirectory
