{-# Language OverloadedStrings #-}  -- Needed for resolveReference

module Repositories (
    run
) where

import Conduit (runConduit, (.|), sinkList)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (foldMap)
import Data.Tagged
import Data.Text (Text)
import Git
import Git.Types (RefTarget)
import Git.Libgit2 (lgFactory, LgRepo)
import qualified Data.HashMap.Strict as HashMap
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.IO.Error (tryIOError)

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
processRepo templates outputDirectory path = withRepository lgFactory path $
    processRepo' templates outputDirectory path

-- This is split out to make type reasoning a bit easier.
processRepo' :: [Template] -> FilePath -> FilePath -> ReaderT LgRepo IO ()
processRepo' templates outputDirectory path = do
    liftIO $ createDirectoryIfMissing True outPath
    ref <- resolveReference "HEAD"
    case ref of
        Nothing -> liftIO . print $ "gitserve: " <> name <> ": Failed to resolve HEAD."
        Just commitID -> do
            let head = Tagged commitID
            -- Variables available in the ginger templates --

            -- description: The description of the repository from repo/description, if
            -- it exists
            description <- liftIO $ getDescription $ outPath </> "description"

            -- commits: A list of `Git.Commit` objects to HEAD.
            commits <- getCommits head

            -- tree: A list of `(TreeFilePath, TreeEntry r)` objects at HEAD.
            tree <- getTree head

            -- Run the generator --
            let repo = package description commits tree
            liftIO . sequence . map (generate repo) $ templates
            return ()
  where
    name = takeFileName path
    outPath = outputDirectory </> name

getCommits :: CommitOid LgRepo -> ReaderT LgRepo IO [Commit LgRepo]
getCommits commitID = sequence . fmap loadCommit <=<
    runConduit $ sourceObjects Nothing commitID False .| sinkList

loadCommit :: ObjectOid LgRepo -> ReaderT LgRepo IO (Commit LgRepo)
loadCommit (CommitObjOid oid) = lookupCommit oid

getTree :: CommitOid LgRepo -> ReaderT LgRepo IO [(TreeFilePath, TreeEntry LgRepo)]
getTree commitID = do
    head <- lookupCommit commitID
    lookupTree (commitTree head) >>= listTreeEntries

getDescription :: FilePath -> IO String
getDescription path = either (const "") id <$> tryIOError (readFile path)

package description commits tree = HashMap.fromList
    [ ("commits", "commits")
    , ("description", "description")
    , ("tree", "tree")
    ]
