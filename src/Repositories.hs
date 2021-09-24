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
import Data.Text (pack, Text, strip, breakOn)
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

import Config (Config, repoPaths, outputDirectory, host)
import Templates (Template, generate)

{-
This is the entrypoint that receives the ``Config`` and uses it to map over our
repositories, reading from them and writing out their web pages using the given
templates.
-}
run :: Config -> [Template] -> IO ()
run config templates = foldMap (processRepo templates config) . repoPaths $ config

----------------------------------------------------------------------------------------

{-
This receives a file path to a single repository and tries to process it. If the
repository doesn't exist or is unreadable in any way we can forget about it and move on
(after informing the user of course).
-}
processRepo :: [Template] -> Config -> FilePath -> IO ()
processRepo templates config path = withRepository lgFactory path $
    processRepo' templates config path

-- This is split out to make type reasoning a bit easier.
processRepo' :: [Template] -> Config -> FilePath -> ReaderT LgRepo IO ()
processRepo' templates config path = do
    let name = takeFileName path
    liftIO $ createDirectoryIfMissing True $ outputDirectory config </> name
    resolveReference "HEAD" >>= \case
        Nothing -> liftIO . print $ "gitserve: " <> name <> ": Failed to resolve HEAD."
        Just commitID -> do
            let gitHead = Tagged commitID
            -- Variables available in the ginger templates: --

            -- description: The description of the repository from repo/description, if
            -- it exists.
            description <- liftIO $ getDescription $ path </> "description"

            -- commits: A list of `Git.Commit` objects to HEAD.
            commits <- getCommits gitHead

            -- tree: A list of `(TreeFilePath, TreeEntry r)` objects at HEAD.
            tree <- getTree gitHead

            -- Run the generator --
            let repo = package config name description commits tree
            liftIO . mapM (generate repo) $ templates
            return ()

{-
The role of the function above is to gather information about a git repository and
package it all together in such a way that various parts can be accessed and used by
Ginger templates. `package` takes these pieces of information and places it all into a
hashmap which Ginger can use to look up variables.
-}
package
    :: Config
    -> FilePath
    -> Text
    -> [Commit LgRepo]
    -> [TreeFile]
    -> HashMap.HashMap Text (GVal (Run SourcePos IO Html))
package config name description commits tree = HashMap.fromList
    [ ("host", toGVal $ host config)
    , ("name", toGVal $ pack name)
    , ("description", toGVal description)
    , ("commits", toGVal commits)
    , ("tree", toGVal ("tree" :: String))
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

getDescription :: FilePath -> IO Text
getDescription path = fromRight "" <$> tryIOError (pack <$> readFile path)

{-
Here we define how commits can be accessed and represented in Ginger templates.

This is an orphan instance but we can let it slide.
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
