{-# LANGUAGE OverloadedStrings #-}

module Index (
    runIndex,
) where

import Control.Monad (void)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, unpack)
import System.FilePath ((</>))
import Text.Ginger.GVal (GVal, toGVal)
import Text.Ginger.Html (Html, htmlSource)
import Text.Ginger.Parse (SourcePos)
import Text.Ginger.Run (Run, easyRenderM)

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
        Just template -> do
            putStrLn $ "Writing " <> output
            writeFile output "" -- Clear contents of file if it exists
            void $
                easyRenderM
                    (appendFile output . unpack . htmlSource)
                    (packageIndex config repos)
                    (templateGinger template)
          where
            config = envConfig env
            output = outputDirectory config </> "index.html"

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
