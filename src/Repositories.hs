-- Needed for the Target class type constraints
{-# LANGUAGE FlexibleContexts #-}
-- Needed for the Target class type constraints
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repositories (
    run,
) where

import qualified Bindings.Libgit2 as LG
import Conduit (runConduit, sinkList, (.|))
import Control.Exception (try)
import Control.Monad (filterM, unless, when, (<=<))
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.Either (isRight)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import Data.Tagged (Tagged (..), untag)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CFloat, CInt, CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import qualified Git
import Git.Libgit2 (LgRepo, lgDiffTreeToTree, lgFactory)
import Path (Abs, Dir, File, Path, Rel, dirname, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir)
import qualified System.Directory as D
import qualified System.FilePath as FP
import Text.Ginger.GVal (GVal, ToGVal, toGVal)

import Env (Env (..))
import Templates (Template (..), generate)
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
    okRepo :: Path Abs Dir -> IO (Either Git.GitException LgRepo)
    okRepo p =
        try . liftIO . Git.openRepository lgFactory $
            Git.defaultRepositoryOptions{Git.repoPath = toFilePath p}

{-
Pass the repository's folder, get its description. The algorithm is:

    1. Check for a description in repo/description
    2. Check for a description in repo/.git/description
    3. Fallback to the repo folder's name.

-}
getDescription :: Path Abs Dir -> IO Text
getDescription dir =
    ifM
        (D.doesFileExist inTop)
        (return . Just $ inTop)
        ( ifM
            (D.doesFileExist inGit)
            (return . Just $ inGit)
            (return Nothing)
        )
        >>= \case
            Just file ->
                T.strip . T.pack <$> readFile file
            Nothing ->
                return . T.pack . toFilePath . dirname $ dir
  where
    inTop = toFilePath dir FP.</> "description"
    inGit = toFilePath dir FP.</> ".git" FP.</> "description"

{-
This receives a file path to a single repository and tries to process it. If the
repository doesn't exist or is unreadable in any way we can forget about it and move on
(after informing the user of course).
-}
processRepo :: Env -> [Repo] -> Repo -> IO Repo
processRepo env repos repo =
    Git.withRepository lgFactory (toFilePath . repositoryPath $ repo) $ processRepo' env repos repo

processRepo' :: Env -> [Repo] -> Repo -> ReaderT LgRepo IO Repo
processRepo' env repos repo = do
    let name = dirname . repositoryPath $ repo
    let output = envOutput env </> name

    Git.resolveReference "HEAD" >>= \case
        Nothing -> do
            liftIO . unless (envQuiet env) . putStrLn $ "gitserve: " <> show name <> ": Failed to resolve HEAD."
            return repo
        Just commitID -> do
            let gitHead = Tagged commitID
            headCommit <- loadDiff =<< Git.lookupCommit gitHead

            -- If a page exists for the head commit, don't do anything else --
            exists <-
                liftIO . D.doesFileExist $
                    toFilePath output FP.</> "commit" FP.</> show commitID <> ".html"

            when (not exists || envForce env) $ do
                -- Collect variables available to the ginger templates --
                commits <- getCommits gitHead
                tree <- getTree gitHead
                tags <- getRefs "refs/tags/"
                branches <- getRefs "refs/heads/"
                let scope = package env repos name (repositoryDescription repo) commits tree tags branches

                -- Create the destination folders --
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

                -- Copy any static files/folders into the output folder --
                liftIO . envRepoCopyStatics env $ output

            return
                repo{repositoryHead = Just headCommit}

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
    [Commit] ->
    [TreeFile] ->
    [Ref] ->
    [Ref] ->
    HashMap.HashMap Text (GVal RunRepo)
package env repos name description commits tree tags branches =
    HashMap.fromList
        [ ("host", toGVal . envHost $ env)
        , ("repositories", toGVal repos)
        , ("name", toGVal . T.pack . init . toFilePath $ name)
        , ("description", toGVal description)
        , ("commits", toGVal commits)
        , ("tree", toGVal . filter (notElem FP.pathSeparator . T.unpack . treeFilePath) $ tree)
        , ("tree_recursive", toGVal tree)
        , ("tags", toGVal tags)
        , ("branches", toGVal branches)
        , ("readme", toGVal . findFile "readme" $ tree)
        , ("license", toGVal . findFile "license" $ tree)
        ]

{-
Collect commit information.
-}
getCommits :: Git.CommitOid LgRepo -> ReaderT LgRepo IO [Commit]
getCommits commitID =
    fmap reverse . sequence . mapMaybe loadCommit
        <=< runConduit
        $ Git.sourceObjects Nothing commitID False .| sinkList

loadCommit :: Git.ObjectOid LgRepo -> Maybe (ReaderT LgRepo IO Commit)
loadCommit (Git.CommitObjOid oid) = Just $ loadDiff =<< Git.lookupCommit oid
loadCommit _ = Nothing

loadDiff :: Git.Commit LgRepo -> ReaderT LgRepo IO Commit
loadDiff gitCommit = do
    let tree = Git.commitTree gitCommit
    newTree <- Git.lookupTree tree
    oldTree <-
        case listToMaybe . Git.commitParents $ gitCommit of
            Just parent ->
                fmap Just . Git.lookupTree . Git.commitTree =<< Git.lookupCommit parent
            Nothing ->
                return Nothing

    ioref <- liftIO . newIORef $ []
    lgDiffTreeToTree
        (fileCallback ioref)
        (hunkCallback ioref)
        (dataCallback ioref)
        oldTree
        (Just newTree)
    diffs <- liftIO . readIORef $ ioref

    return . Commit gitCommit . fmap fixupLists $ diffs
  where
    fileCallback ::
        IORef [Diff] ->
        Ptr LG.C'git_diff_delta ->
        CFloat ->
        Ptr () ->
        IO CInt
    fileCallback ioref dPtr _progress _payload = do
        delta <- peek dPtr
        newFile <- B.packCString . LG.c'git_diff_file'path . LG.c'git_diff_delta'new_file $ delta
        oldFile <- B.packCString . LG.c'git_diff_file'path . LG.c'git_diff_delta'old_file $ delta
        let oldFile' = bool Nothing (Just oldFile) (newFile /= oldFile)
            status = toEnum . fromIntegral . LG.c'git_diff_delta'status $ delta
            diff = Diff newFile oldFile' status []
        modifyIORef ioref (diff :)
        return 0

    hunkCallback ::
        IORef [Diff] ->
        Ptr LG.C'git_diff_delta ->
        Ptr LG.C'git_diff_range ->
        CString ->
        CSize ->
        Ptr () ->
        IO CInt
    hunkCallback ioref _dPtr _range header headerLen _payload = do
        (cur : _, rest) <- splitAt 1 <$> readIORef ioref
        bs <- curry B.packCStringLen header (fromIntegral headerLen)
        let hunk = Hunk bs []
        writeIORef ioref $ cur{diffHunks = hunk : diffHunks cur} : rest
        return 0

    dataCallback ::
        IORef [Diff] ->
        Ptr LG.C'git_diff_delta ->
        Ptr LG.C'git_diff_range ->
        CChar ->
        CString ->
        CSize ->
        Ptr () ->
        IO CInt
    dataCallback ioref _dPtr _range lineOrigin content contentLen _payload = do
        bs <- curry B.packCStringLen content (fromIntegral contentLen)
        let bs' = B.cons (fromIntegral lineOrigin) bs
        (cur : _, rest) <- splitAt 1 <$> readIORef ioref
        let (curHunk : _, restHunks) = splitAt 1 . diffHunks $ cur
        let updated =
                cur
                    { diffHunks =
                        curHunk
                            { hunkLines = bs' : hunkLines curHunk
                            } :
                        restHunks
                    }
        writeIORef ioref $ updated : rest
        return 0

    -- The callbacks prepend and then we put the lists the right way round here.
    -- This avoids traversing the lists every time we add an item.
    fixupLists :: Diff -> Diff
    fixupLists diff = diff{diffHunks = fmap fixupLines . reverse . diffHunks $ diff}
      where
        fixupLines hunk = hunk{hunkLines = reverse . hunkLines $ hunk}

{-
Collect tree information for the given commit. Recurses on directories to list their
contents.
-}
getTree :: Git.CommitOid LgRepo -> ReaderT LgRepo IO [TreeFile]
getTree = getTree' "" . Git.commitTree <=< Git.lookupCommit

getTree' :: Git.TreeFilePath -> Git.TreeOid LgRepo -> ReaderT LgRepo IO [TreeFile]
getTree' parent toid = do
    entries <- Git.listTreeEntries =<< Git.lookupTree toid
    let entries' = fmap (prependParent parent) entries
    contents <- mapM getEntryContents entries'
    modes <- mapM (getEntryModes . snd) entries'
    return $ zipWith3 TreeFile (fmap treePaths entries') contents modes

prependParent :: Git.TreeFilePath -> (Git.TreeFilePath, Git.TreeEntry LgRepo) -> (Git.TreeFilePath, Git.TreeEntry LgRepo)
prependParent "" pathentry = pathentry
prependParent parent (path, entry) = (mconcat [parent, "/", path], entry)

getEntryContents :: (Git.TreeFilePath, Git.TreeEntry LgRepo) -> ReaderT LgRepo IO TreeFileContents
getEntryContents (_, Git.BlobEntry oid _) = getBlobContents oid
getEntryContents (path, Git.TreeEntry oid) = FolderContents <$> getTree' path oid
getEntryContents (_, Git.CommitEntry oid) = return . FileContents . T.pack . show . untag $ oid

getEntryModes :: Git.TreeEntry LgRepo -> ReaderT LgRepo IO TreeEntryMode
getEntryModes (Git.BlobEntry _ kind) = return . blobkindToMode $ kind
getEntryModes (Git.TreeEntry _) = return ModeDirectory
getEntryModes (Git.CommitEntry _) = return ModeSubmodule

treePaths :: (Git.TreeFilePath, Git.TreeEntry LgRepo) -> Text
treePaths = T.decodeUtf8With T.lenientDecode . fst

{-
Find a file in the tree starting with the specified prefix. The prefix is looked for on
the full path, so will only find files in the top level directory.
-}
findFile :: Text -> [TreeFile] -> Maybe TreeFile
findFile _ [] = Nothing
findFile prefix (f : fs) = if isReadme f then Just f else findFile prefix fs
  where
    isReadme = T.isPrefixOf prefix . T.toLower . treeFilePath

{-
Collect information about references. TODO: Find a more canonical way to split
references into tags or branches rather than filtering the refnames.
-}
getRefs :: Text -> ReaderT LgRepo IO [Ref]
getRefs ref = do
    names <- filter (T.isPrefixOf ref) <$> Git.listReferences
    maybeOids <- mapM Git.resolveReference names
    let names' = catMaybes . zipWith dropName maybeOids $ names
    objs <- mapM Git.lookupObject . catMaybes $ maybeOids
    maybeCommits <- mapM refObjToCommit objs
    let names'' = map (fromJust . T.stripPrefix ref) . catMaybes . zipWith dropName maybeCommits $ names'
    return . zipWith Ref names'' . catMaybes $ maybeCommits
  where
    refObjToCommit ::
        Git.Object LgRepo (ReaderT LgRepo IO) ->
        ReaderT LgRepo IO (Maybe Commit)
    refObjToCommit (Git.CommitObj obj) = Just <$> loadDiff obj
    refObjToCommit _ = return Nothing

    dropName :: Maybe a -> Git.RefName -> Maybe Git.RefName
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

instance Target Commit where
    identify = (++ ".html") . show . untag . Git.commitOid . commitGit
    category = const "commit"

instance Target TreeFile where
    identify = T.unpack . treePathToHref
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
        let scope' = scope <> HashMap.fromList [(T.pack . category $ target, toGVal target)]
        generate output' scope' template
