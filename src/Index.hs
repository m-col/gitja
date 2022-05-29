{-# LANGUAGE OverloadedStrings #-}

module Index (
    runIndex,
) where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Path (toFilePath)
import System.FilePath (combine)
import Text.Ginger.GVal (GVal, toGVal)

import Env (Env (..))
import Templates (Template (..), generateIndex)
import Types

{-
This creates the main index files from the index templates, using information from all
configured respositories.
-}
runIndex :: Env -> [Repo] -> IO ()
runIndex env repos =
    mapM_ (runIndexFile outputDir quiet scope) templates
  where
    outputDir = toFilePath . envOutput $ env
    quiet = envQuiet env
    templates = envIndexTemplates env

    scope :: HashMap.HashMap T.Text (GVal RunRepo)
    scope =
        HashMap.fromList
            [ ("host", toGVal $ envHost env)
            , ("repositories", toGVal repos)
            ]

{-
Use the scope created above to render a single index template.
-}
runIndexFile ::
    FilePath ->
    Bool ->
    HashMap.HashMap T.Text (GVal RunRepo) ->
    Template ->
    IO ()
runIndexFile outputDir quiet scope template = do
    let output = combine outputDir . toFilePath . templatePath $ template
    unless quiet . putStrLn $ "Writing " <> output
    TL.writeFile output =<< generateIndex template scope
