{-# LANGUAGE OverloadedStrings #-}

module Index (
    runIndex,
) where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import Path (toFilePath)
import System.FilePath (combine)
import Text.Ginger.GVal (GVal, toGVal)

import Env (Env (..))
import Templates (Template (..), generate)
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
    TL.writeFile output =<< generate template (packageIndex env repos)

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
