{-# LANGUAGE OverloadedStrings #-}

module Index (
    runIndex,
) where

import Control.Monad (unless, void)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, unpack)
import Path (toFilePath)
import Path.IO (ignoringAbsence)
import System.Directory (removeFile)
import System.FilePath (combine)
import Text.Ginger.GVal (GVal, toGVal)
import Text.Ginger.Html (htmlSource)
import Text.Ginger.Run (easyRenderM)

import Env (Env (..))
import Templates (Template (..))
import Types

{-
This creates the main index files from the index templates, using information from all
configured respositories.
-}
runIndex :: Env -> [Repo] -> IO ()
runIndex env repos = mapM_ (runIndexFile env repos) . envIndexTemplates $ env

runIndexFile :: Env -> [Repo] -> Template -> IO ()
runIndexFile env repos template = do
    let output = combine (toFilePath . envOutput $ env) . toFilePath . templatePath $ template
    unless (envQuiet env) . putStrLn $ "Writing " <> output
    ignoringAbsence . removeFile $ output
    void $
        easyRenderM
            (appendFile output . unpack . htmlSource)
            (packageIndex env repos)
            (templateGinger template)

{-
This packages the variables that are available inside the index scope.
-}
packageIndex ::
    Env ->
    [Repo] ->
    HashMap.HashMap Text (GVal RunRepo)
packageIndex env repos =
    HashMap.fromList
        [ ("host", toGVal $ envHost env)
        , ("repositories", toGVal repos)
        ]
