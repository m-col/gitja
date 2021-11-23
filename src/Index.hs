{-# LANGUAGE OverloadedStrings #-}

module Index (
    runIndex,
) where

import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import System.FilePath ((</>))
import Text.Ginger.GVal (GVal, toGVal)
import Text.Ginger.Html (Html)
import Text.Ginger.Parse (SourcePos)
import Text.Ginger.Run (Run)

import Config
import Templates
import Types

{-
This creates the main index file from the index template, using information from all
configured respositories.
-}
runIndex :: Env -> [Repo] -> IO ()
runIndex env repos =
    case envIndexTemplate env of
        Nothing ->
            return ()
        Just template ->
            generate
                (outputDirectory config </> "index.html")
                (packageIndex config repos)
                template
          where
            config = envConfig env

----------------------------------------------------------------------------------------
-- Private -----------------------------------------------------------------------------

{-
This packages the variables that are available inside the index template.
-}
packageIndex ::
    Config ->
    [Repo] ->
    HashMap.HashMap Text (GVal (Run SourcePos IO Html))
packageIndex config repos =
    HashMap.fromList
        [ ("host", toGVal $ host config)
        , ("repositories", toGVal repos)
        ]
