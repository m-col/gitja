{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}  -- Needed for resolveReference

module Repositories (
    run
) where

import Conduit (runConduit, (.|), sinkList)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (fromRight)
import Data.Tagged
import Data.Text (pack, Text)
import Data.Maybe (catMaybes)
import Git
import Git.Libgit2 (lgFactory, LgRepo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.IO.Error (tryIOError)
import qualified Data.HashMap.Strict as HashMap

import Config (Config, repoPaths, outputDirectory)
import Templates (Template, generate)

{-
This is the entrypoint that receives the ``Config`` and uses it to map over our
repositories, reading from them and writing out their web pages using the given
templates.
-}
run :: Config -> [Template] -> IO ()
run config templates = foldMap (processRepo templates $ outputDirectory config) . repoPaths $ config

----------------------------------------------------------------------------------------

{-
This receives a file path to a single repository and tries to process it. If the
repository doesn't exist or is unreadable in any way we can forget about it and move on
(after informing the user of course).
-}
processRepo :: [Template] -> FilePath -> FilePath -> IO ()
processRepo templates directory path = withRepository lgFactory path $
    processRepo' templates directory path

-- This is split out to make type reasoning a bit easier.
processRepo' :: [Template] -> FilePath -> FilePath -> ReaderT LgRepo IO ()
processRepo' templates directory path = do
    liftIO $ createDirectoryIfMissing True outPath
    resolveReference "HEAD" >>= \case
        Nothing -> liftIO . print $ "gitserve: " <> name <> ": Failed to resolve HEAD."
        Just commitID -> do
            let gitHead = Tagged commitID
            -- Variables available in the ginger templates: --

            -- description: The description of the repository from repo/description, if
            -- it exists.
            description <- liftIO $ getDescription $ outPath </> "description"

            -- commits: A list of `Git.Commit` objects to HEAD.
            commits <- getCommits gitHead

            -- tree: A list of `(TreeFilePath, TreeEntry r)` objects at HEAD.
            tree <- getTree gitHead

            -- Run the generator --
            let repo = package description commits tree
            liftIO . mapM (generate repo) $ templates
            return ()
  where
    name = takeFileName path
    outPath = directory </> name

getCommits :: CommitOid LgRepo -> ReaderT LgRepo IO [Commit LgRepo]
getCommits commitID = do
    oids <- runConduit $ sourceObjects Nothing commitID False .| sinkList
    sequence . catMaybes $ loadCommit <$> oids

loadCommit :: ObjectOid LgRepo -> Maybe (ReaderT LgRepo IO (Commit LgRepo))
loadCommit (CommitObjOid oid) = Just $ lookupCommit oid
loadCommit _ = Nothing

getTree :: CommitOid LgRepo -> ReaderT LgRepo IO [(TreeFilePath, TreeEntry LgRepo)]
getTree commitID = do
    gitHead <- lookupCommit commitID
    lookupTree (commitTree gitHead) >>= listTreeEntries

getDescription :: FilePath -> IO Text
getDescription path = fromRight "" <$> tryIOError (pack <$> readFile path)

package
    :: Text
    -> [Commit LgRepo]
    -> [(TreeFilePath, TreeEntry r)]
    -> HashMap.HashMap Text Text
package description commits tree = HashMap.fromList
    [ ("commits", "commits")
    , ("description", description)
    , ("tree", "tree")
    ]
