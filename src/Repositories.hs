{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}  -- Needed for resolveReference
{-# Language FlexibleInstances #-}  -- Needed for `instance ToGVal`
{-# Language MultiParamTypeClasses #-}  -- Needed for `instance ToGVal`
{-# Language InstanceSigs #-}  -- Needed for toGVal type signature

module Repositories (
    run
) where

import Conduit (runConduit, (.|), sinkList)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Default (def)
import Data.Either (fromRight)
import Data.Tagged
import Data.Text (pack, unpack, Text, strip, breakOn, replace, append)
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (mapMaybe)
import Git
import Git.Libgit2 (lgFactory, LgRepo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.IO.Error (tryIOError)
import Text.Ginger.GVal (GVal, toGVal, ToGVal, asText, asHtml, asLookup)
import Text.Ginger.Html (Html, html)
import Text.Ginger.Run (Run)
import Text.Ginger.Parse (SourcePos)
import qualified Data.HashMap.Strict as HashMap

import Config
import Templates

{-
This is the entrypoint that receives the ``Config`` and uses it to map over our
repositories, reading from them and writing out their web pages using the given
templates.
-}
run :: Env -> IO ()
run env = do
    foldMap (processRepo env) . repoPaths . envConfig $ env  -- TODO: make concurrent
    runIndex (envConfig env) (envIndexTemplate env)

----------------------------------------------------------------------------------------
-- Private -----------------------------------------------------------------------------

{-
This receives a file path to a single repository and tries to process it. If the
repository doesn't exist or is unreadable in any way we can forget about it and move on
(after informing the user of course).
-}
processRepo :: Env -> FilePath -> IO ()
processRepo env path = withRepository lgFactory path $ processRepo' env path

processRepo' :: Env -> FilePath -> ReaderT LgRepo IO ()
processRepo' env path = do
    let name = takeFileName path
    let output = outputDirectory (envConfig env) </> name
    liftIO $ createDirectoryIfMissing True output
    resolveReference "HEAD" >>= \case
        Nothing -> liftIO . print $ "gitserve: " <> name <> ": Failed to resolve HEAD."
        Just commitID -> do
            let gitHead = Tagged commitID
            -- Variables available in the ginger templates: --

            -- description: The description of the repository from repo/description, if
            -- it exists.
            description <- liftIO . getDescription $ path

            -- commits: A list of `Git.Commit` objects to HEAD.
            commits <- getCommits gitHead

            -- tree: A list of `TreeFile` objects at HEAD.
            tree <- getTree gitHead

            -- Run the generator --
            let repo = package env name description commits tree
            liftIO . mapM (generate output repo) $ envTemplates env
            let commitScope = packageCommit env name description
            liftIO . mapM (generateCommit output commitScope $ envCommitTemplate env) $ commits
            let fileScope = packageFile env name description
            liftIO . mapM (generateFile output fileScope $ envFileTemplate env) $ tree
            return ()

{-
The role of the function above is to gather information about a git repository and
package it all together in such a way that various parts can be accessed and used by
Ginger templates. `package` takes these pieces of information and places it all into a
hashmap which Ginger can use to look up variables.
-}
package
    :: Env
    -> FilePath
    -> Text
    -> [Commit LgRepo]
    -> [TreeFile]
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
package env name description commits tree = HashMap.fromList
    [ ("host", toGVal . host . envConfig $ env)
    , ("name", toGVal . pack $ name)
    , ("description", toGVal description)
    , ("commits", toGVal . reverse $ commits)  -- Could be optimised
    , ("tree", toGVal tree)
    ]

getCommits :: CommitOid LgRepo -> ReaderT LgRepo IO [Commit LgRepo]
getCommits commitID =
    sequence . mapMaybe loadCommit <=<
    runConduit $ sourceObjects Nothing commitID False .| sinkList

loadCommit :: ObjectOid LgRepo -> Maybe (ReaderT LgRepo IO (Commit LgRepo))
loadCommit (CommitObjOid oid) = Just $ lookupCommit oid
loadCommit _ = Nothing

data TreeFile = TreeFile
    { treeFilePath :: TreeFilePath
    , treeEntry :: TreeEntry LgRepo
    }

getTree :: CommitOid LgRepo -> ReaderT LgRepo IO [TreeFile]
getTree commitID = do
    entries <- listTreeEntries =<< lookupTree . commitTree =<< lookupCommit commitID
    return $ uncurry TreeFile <$> entries

-- Pass the repository's folder, get its description.
getDescription :: FilePath -> IO Text
getDescription = fmap (fromRight "") . tryIOError . fmap pack . readFile . flip (</>) "description"

{-
Here we define how commits can be accessed and represented in Ginger templates.
-}
instance ToGVal m (Commit LgRepo) where
    toGVal :: Commit LgRepo -> GVal m
    toGVal commit = def
        { asHtml = html . pack . show . commitLog $ commit
        , asText = pack . show . commitLog $ commit
        , asLookup = Just . commitAsLookup $ commit
        }

commitAsLookup :: Commit LgRepo -> Text -> Maybe (GVal m)
commitAsLookup commit = \case
    "id" -> Just . toGVal . renderObjOid . commitOid $ commit
    "title" -> Just . toGVal . strip . fst . breakOn "\n" . commitLog $ commit
    "body" -> Just . toGVal . strip . snd . breakOn "\n" . commitLog $ commit
    "message" -> Just . toGVal . strip . commitLog $ commit
    "author" -> Just . toGVal . strip . signatureName . commitAuthor $ commit
    "committer" -> Just . toGVal . strip . signatureName . commitCommitter $ commit
    "author_email" -> Just . toGVal . strip . signatureEmail . commitAuthor $ commit
    "committer_email" -> Just . toGVal . strip . signatureEmail . commitCommitter $ commit
    "authored" -> Just . toGVal . show . signatureWhen . commitAuthor $ commit
    "committed" -> Just . toGVal . show . signatureWhen . commitCommitter $ commit
    "encoding" -> Just . toGVal . strip . commitEncoding $ commit
    _ -> Nothing


{-
Here we define how files in the tree can be accessed by Ginger templates.
-}
instance ToGVal m TreeFile where
    toGVal :: TreeFile -> GVal m
    toGVal treefile = def
        { asHtml = html . pack . show . treeFilePath $ treefile
        , asText = pack . show . treeFilePath $ treefile
        , asLookup = Just . treeAsLookup $ treefile
        }

treeAsLookup :: TreeFile -> Text -> Maybe (GVal m)
treeAsLookup treefile = \case
    "path" -> Just . toGVal . treeFilePath $ treefile
    "href" -> Just . toGVal . flip append ".html" . replace "/" "." . decodeUtf8 . treeFilePath $ treefile
    _ -> Nothing

----------------------------------------------------------------------------------------

{-
This creates the main index file from the index template, using information from all
configured respositories.
-}
runIndex :: Config -> Maybe Template -> IO ()
runIndex _ Nothing = return ()
runIndex config (Just template) = do
    let paths = repoPaths config
    descriptions <- mapM getDescription paths
    let repos = zipWith ($) (Repo . takeFileName <$> paths) descriptions
    let indexScope = packageIndex config repos
    generate (outputDirectory config) indexScope (template { templatePath = "index.html" })
    return ()

packageIndex
    :: Config
    -> [Repo]
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
packageIndex config repos = HashMap.fromList
    [ ("host", toGVal $ host config)
    , ("repositories", toGVal repos)
    ]

{-
The index template can access variables in the index scope. The primary variable here is
the list of repositories, which can be looped over and each repo entry has some
properties that can be accessed. These are defined here.
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

----------------------------------------------------------------------------------------

generateCommit
    :: FilePath
    -> (Commit LgRepo -> HashMap.HashMap Text (GVal (Run SourcePos IO Html)))
    -> Maybe Template
    -> Commit LgRepo
    -> IO ()
generateCommit _ _ Nothing _ = return ()
generateCommit output scope (Just template) commit = do
    let hash = unpack . renderObjOid . commitOid $ commit
    let template' = template { templatePath = hash ++ ".html" }
    liftIO $ createDirectoryIfMissing True (output </> "commits")
    generate (output </> "commits") (scope commit) template'
    return ()

packageCommit
    :: Env
    -> FilePath
    -> Text
    -> Commit LgRepo
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
packageCommit env name description commit = HashMap.fromList
    [ ("host", toGVal . host . envConfig $ env)
    , ("name", toGVal . pack $ name)
    , ("description", toGVal description)
    , ("commit", toGVal commit)
    ]

-- TODO: DRY

generateFile
    :: FilePath
    -> (TreeFile -> HashMap.HashMap Text (GVal (Run SourcePos IO Html)))
    -> Maybe Template
    -> TreeFile
    -> IO ()
generateFile _ _ Nothing _ = return ()
generateFile output scope (Just template) file = do
    let path = unpack . replace "/" "." . decodeUtf8 . treeFilePath $ file
    let template' = template { templatePath = path ++ ".html" }
    liftIO $ createDirectoryIfMissing True (output </> "files")
    generate (output </> "files") (scope file) template'
    return ()

packageFile
    :: Env
    -> FilePath
    -> Text
    -> TreeFile
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
packageFile env name description file = HashMap.fromList
    [ ("host", toGVal . host . envConfig $ env)
    , ("name", toGVal . pack $ name)
    , ("description", toGVal description)
    , ("file", toGVal file)
    ]
