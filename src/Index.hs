{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}  -- Needed for `instance ToGVal`
{-# Language MultiParamTypeClasses #-}  -- Needed for `instance ToGVal`
{-# Language InstanceSigs #-}  -- Needed for toGVal type signature

module Index (
    runIndex,
    Repo,
    repoAsLookup,
    repoPath,
    repoDescription,
) where

import Data.Default (def)
import Data.Either (fromRight)
import Data.Text (pack, Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.IO.Error (tryIOError)
import Text.Ginger.GVal (GVal, toGVal, ToGVal, asText, asHtml, asLookup)
import Text.Ginger.Html (Html, html)
import Text.Ginger.Parse (SourcePos)
import Text.Ginger.Run (Run)
import qualified Data.HashMap.Strict as HashMap

import Config
import Templates

{-
This creates the main index file from the index template, using information from all
configured respositories.
-}
runIndex :: Env -> IO [Repo]
runIndex env =
    case envIndexTemplate env of
        Nothing ->
            return []
        Just template -> do
            let config = envConfig env
            createDirectoryIfMissing True $ outputDirectory config
            let output = (outputDirectory config) </> "index.html"
            repos <- loadRepos config
            let scope = packageIndex config repos
            generate output scope template
            return repos

{-
The unique variable in the index scope is the list of repositories, which can be looped
over and each entry has some properties that can be accessed. Those are defined here.
The type is exported so that within the scope of a single repository, its name and
description is available.
-}
data Repo = Repo
    { repoPath :: FilePath
    , repoDescription :: Text
    }

instance ToGVal m Repo where
    toGVal :: Repo -> GVal m
    toGVal repo = def
        { asHtml = html . pack . show . takeFileName . repoPath $ repo
        , asText = pack . show . takeFileName . repoPath $ repo
        , asLookup = Just . repoAsLookup $ repo
        }

repoAsLookup :: Repo -> Text -> Maybe (GVal m)
repoAsLookup repo = \case
    "name" -> Just . toGVal . takeFileName . repoPath $ repo
    "description" -> Just . toGVal . repoDescription $ repo
    _ -> Nothing


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
Get paths along with their descriptions.
-}
loadRepos :: Config -> IO [Repo]
loadRepos config = zipWith ($) (Repo <$> paths) <$> mapM getDescription paths
  where
    paths = repoPaths config

{-
Pass the repository's folder, get its description.
-}
getDescription :: FilePath -> IO Text
getDescription = fmap (fromRight "") . tryIOError . fmap pack . readFile . (</> "description")
