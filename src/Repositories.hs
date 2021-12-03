-- Needed for the Target class type constraints
{-# LANGUAGE FlexibleContexts #-}
-- Needed for the Target class type constraints
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repositories (
    run,
) where

import Conduit (runConduit, sinkList, (.|))
import Control.Exception (try)
import Control.Monad (filterM, unless, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (fromRight, isRight)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Tagged
import Data.Text (Text, isPrefixOf, pack, strip, stripPrefix, toLower, unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Git
import Git.Libgit2 (LgRepo, lgFactory)
import Path (Abs, Dir, File, Path, Rel, dirname, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir)
import qualified System.FilePath as FP
import System.IO.Error (tryIOError)
import Text.Ginger.GVal (GVal, ToGVal, toGVal)

import Templates (Env (..), Template (..), generate)
import Types

{-
This is the entrypoint maps over our repositories, reading from them and writing out
their web pages using the loaded templates.
-}
run :: Env -> IO [Repo]
run env = do
    repos <- loadRepos env
    mapM (processRepo env repos) repos

{-
Get paths along with their descriptions.
-}
loadRepos :: Env -> IO [Repo]
loadRepos env = do
    paths' <- filterM (fmap isRight . okRepo) . envRepos $ env
    descs <- mapM getDescription paths'
    return . fmap ($ Nothing) . zipWith Repo paths' $ descs
  where
    okRepo :: Path Abs Dir -> IO (Either GitException LgRepo)
    okRepo p = try . liftIO . openRepository lgFactory $ defaultRepositoryOptions{repoPath = toFilePath p}

{-
Pass the repository's folder, get its description.
-}
getDescription :: Path Abs Dir -> IO Text
getDescription = fmap (fromRight "") . tryIOError . fmap (strip . pack) . readFile . flip FP.combine "description" . toFilePath

{-
This receives a file path to a single repository and tries to process it. If the
repository doesn't exist or is unreadable in any way we can forget about it and move on
(after informing the user of course).
-}
processRepo :: Env -> [Repo] -> Repo -> IO Repo
processRepo env repos repo =
    withRepository lgFactory (toFilePath . repositoryPath $ repo) $ processRepo' env repos repo

processRepo' :: Env -> [Repo] -> Repo -> ReaderT LgRepo IO Repo
processRepo' env repos repo = do
    let name = dirname . repositoryPath $ repo
    let output = envOutput env </> name

    resolveReference "HEAD" >>= \case
        Nothing -> do
            liftIO . unless (envQuiet env) . putStrLn $ "gitserve: " <> show name <> ": Failed to resolve HEAD."
            return repo
        Just commitID -> do
            let gitHead = Tagged commitID

            -- Collect variables available in the ginger templates --
            commits <- getCommits gitHead
            tree <- getTree gitHead
            tags <- getRefs "refs/tags/"
            branches <- getRefs "refs/heads/"
            let scope = package env repos name (repositoryDescription repo) commits tree tags branches

            -- Create the destination folders
            commitDir <- liftIO . parseRelDir $ "commit"
            fileDir <- liftIO . parseRelDir $ "file"
            liftIO . ensureDir $ output </> commitDir
            liftIO . ensureDir $ output </> fileDir

            -- Run the generator --
            let quiet = envQuiet env
            let force = envForce env
            mapM_ (genRepo output scope) $ envRepoTemplates env
            mapM_ (genTarget output scope quiet force $ envCommitTemplate env) commits
            mapM_ (genTarget output scope quiet True $ envFileTemplate env) tree -- TODO: detect file changes

            -- Return the repo with the head so the index page can use it. --
            return
                repo{repositoryHead = Just . head $ commits}

{-
The role of the function above is to gather information about a git repository and
package it all together in such a way that various parts can be accessed and used by
Ginger templates. `package` takes these pieces of information and places it all into a
hashmap which Ginger can use to look up variables.
-}
package ::
    Env ->
    [Repo] ->
    Path Rel Dir ->
    Text ->
    [Commit LgRepo] ->
    [TreeFile] ->
    [Ref] ->
    [Ref] ->
    HashMap.HashMap Text (GVal RunRepo)
package env repos name description commits tree tags branches =
    HashMap.fromList
        [ ("host", toGVal . envHost $ env)
        , ("repositories", toGVal repos)
        , ("name", toGVal . pack . init . toFilePath $ name)
        , ("description", toGVal description)
        , ("commits", toGVal . reverse $ commits) -- Could be optimised
        , ("tree", toGVal tree)
        , ("tags", toGVal tags)
        , ("branches", toGVal branches)
        , ("readme", toGVal . findFile "readme" $ tree)
        , ("license", toGVal . findFile "license" $ tree)
        ]

{-
Collect commit information.
-}
getCommits :: CommitOid LgRepo -> ReaderT LgRepo IO [Commit LgRepo]
getCommits commitID =
    sequence . mapMaybe loadCommit
        <=< runConduit
        $ sourceObjects Nothing commitID False .| sinkList

loadCommit :: ObjectOid LgRepo -> Maybe (ReaderT LgRepo IO (Commit LgRepo))
loadCommit (CommitObjOid oid) = Just $ lookupCommit oid
loadCommit _ = Nothing

{-
Collect tree information for the given commit. Recurses on directories to list their
contents.
-}
getTree :: CommitOid LgRepo -> ReaderT LgRepo IO [TreeFile]
getTree = getTree' "" . commitTree <=< lookupCommit

getTree' :: TreeFilePath -> TreeOid LgRepo -> ReaderT LgRepo IO [TreeFile]
getTree' parent toid = do
    entries <- listTreeEntries =<< lookupTree toid
    let entries' = fmap (prependParent parent) entries
    contents <- mapM getEntryContents entries'
    modes <- mapM (getEntryModes . snd) entries'
    return $ zipWith3 TreeFile (fmap fst entries') contents modes

prependParent :: TreeFilePath -> (TreeFilePath, TreeEntry LgRepo) -> (TreeFilePath, TreeEntry LgRepo)
prependParent "" pathentry = pathentry
prependParent parent (path, entry) = (mconcat [parent, "/", path], entry)

getEntryContents :: (TreeFilePath, TreeEntry LgRepo) -> ReaderT LgRepo IO TreeFileContents
getEntryContents (_, BlobEntry oid _) = getBlobContents oid
getEntryContents (path, TreeEntry oid) = FolderContents <$> getTree' path oid
getEntryContents (_, CommitEntry oid) = return . FileContents . pack . show . untag $ oid

getEntryModes :: TreeEntry LgRepo -> ReaderT LgRepo IO TreeEntryMode
getEntryModes (BlobEntry _ kind) = return . blobkindToMode $ kind
getEntryModes (TreeEntry _) = return ModeDirectory
getEntryModes (CommitEntry _) = return ModeSubmodule

{-
Find a file in the tree starting with the specified prefix. The prefix is looked for on
the full path, so will only find files in the top level directory.
-}
findFile :: Text -> [TreeFile] -> Maybe TreeFile
findFile _ [] = Nothing
findFile prefix (f : fs) = if isReadme f then Just f else findFile prefix fs
  where
    isReadme = isPrefixOf prefix . toLower . decodeUtf8With lenientDecode . treeFilePath

{-
Collect information about references. TODO: Find a more canonical way to split
references into tags or branches rather than filtering the refnames.
-}
getRefs :: Text -> ReaderT LgRepo IO [Ref]
getRefs ref = do
    names <- filter (isPrefixOf ref) <$> listReferences
    maybeOids <- mapM resolveReference names
    let names' = catMaybes . zipWith dropName maybeOids $ names
    objs <- mapM lookupObject . catMaybes $ maybeOids
    maybeCommits <- mapM refObjToCommit objs
    let names'' = map (fromJust . stripPrefix ref) . catMaybes . zipWith dropName maybeCommits $ names'
    return . zipWith Ref names'' . catMaybes $ maybeCommits
  where
    refObjToCommit :: Object r (ReaderT LgRepo IO) -> ReaderT LgRepo IO (Maybe (Commit r))
    refObjToCommit (CommitObj obj) = return . Just $ obj
    refObjToCommit _ = return Nothing

    dropName :: Maybe a -> RefName -> Maybe RefName
    dropName (Just _) name = Just name
    dropName Nothing _ = Nothing

genRepo ::
    Path Abs Dir ->
    HashMap.HashMap Text (GVal RunRepo) ->
    Template ->
    ReaderT LgRepo IO ()
genRepo output scope template =
    generate (output </> templatePath template) scope template

----------------------------------------------------------------------------------------
-- Targets -----------------------------------------------------------------------------

{-
A Target refers to a template scope and repository object whose information is available
in that scope. For example, commits are a target as they each generate a scope
containing that commit's information, and these scopes are each rendered in the
commitTemplate. The Target class generalises how each target is represented so that
genTarget can work on any target type.
-}
class ToGVal RunRepo a => Target a where
    identify :: a -> FilePath
    category :: a -> FilePath
    dest :: a -> IO (Path Rel File)
    dest t = do
        dir <- parseRelDir . category $ t
        file <- parseRelFile . identify $ t
        return $ dir </> file

instance Target (Commit LgRepo) where
    identify = (++ ".html") . show . untag . commitOid
    category = const "commit"

instance Target TreeFile where
    identify = unpack . treePathToHref . treeFilePath
    category = const "file"

genTarget ::
    Target a =>
    Path Abs Dir ->
    HashMap.HashMap Text (GVal RunRepo) ->
    Bool ->
    Bool ->
    Maybe Template ->
    a ->
    ReaderT LgRepo IO ()
genTarget _ _ _ _ Nothing _ = return ()
genTarget output scope quiet force (Just template) target = do
    outFile <- liftIO . dest $ target
    let output' = output </> outFile
    exists <- liftIO . doesFileExist $ output'
    when (force || not exists) $ do
        liftIO . unless quiet . putStrLn $ "Writing " <> toFilePath output'
        let scope' = scope <> HashMap.fromList [(pack . category $ target, toGVal target)]
        generate output' scope' template
