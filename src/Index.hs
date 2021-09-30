{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}  -- Needed for `instance ToGVal`
{-# Language MultiParamTypeClasses #-}  -- Needed for `instance ToGVal`
{-# Language InstanceSigs #-}  -- Needed for toGVal type signature

module Index (
    runIndex
) where

import Data.Default (def)
import Data.Text (pack, Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName)
import Text.Ginger.GVal (GVal, toGVal, ToGVal, asText, asHtml, asLookup)
import Text.Ginger.Html (Html, html)
import Text.Ginger.Parse (SourcePos)
import Text.Ginger.Run (Run)
import qualified Data.HashMap.Strict as HashMap

import Config
import Repositories
import Templates

{-
This creates the main index file from the index template, using information from all
configured respositories.
-}
runIndex :: Env -> IO ()
runIndex env =
    case envIndexTemplate env of
        Nothing ->
            return ()
        Just template -> do
            let config = envConfig env
            createDirectoryIfMissing True $ outputDirectory config
            let paths = repoPaths config
            descriptions <- mapM getDescription paths
            let repos = zipWith ($) (Repo . takeFileName <$> paths) descriptions
            let scope = packageIndex config repos
            generate (outputDirectory config) scope (template { templatePath = "index.html" })
            return ()

----------------------------------------------------------------------------------------
-- Private -----------------------------------------------------------------------------

{-
This packages the variables that are available inside the index template.
-}
packageIndex
    :: Config
    -> [Repo]
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
packageIndex config repos = HashMap.fromList
    [ ("host", toGVal $ host config)
    , ("repositories", toGVal repos)
    ]

{-
The unique variable in the index scope is the list of repositories, which can be looped
over and each entry has some properties that can be accessed. Those are defined here.
-}
data Repo = Repo
    { repoName :: FilePath
    , repoDescription :: Text
    }

instance ToGVal m Repo where
    toGVal :: Repo -> GVal m
    toGVal repo = def
        { asHtml = html . pack . show . repoName $ repo
        , asText = pack . show . repoName $ repo
        , asLookup = Just . repoAsLookup $ repo
        }

repoAsLookup :: Repo -> Text -> Maybe (GVal m)
repoAsLookup repo = \case
    "name" -> Just . toGVal . repoName $ repo
    "description" -> Just . toGVal . repoDescription $ repo
    _ -> Nothing
