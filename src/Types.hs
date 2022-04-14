{-# LANGUAGE DerivingStrategies #-}
-- Needed for `instance ToGVal`
{-# LANGUAGE FlexibleInstances #-}
-- Needed for toGVal type signature
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
-- Needed for `instance ToGVal`
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Bindings.Libgit2.Blob (c'git_blob_is_binary, c'git_blob_lookup)
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tagged (untag)
import Data.Text (Text, pack, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Foreign (peek)
import Foreign.ForeignPtr (mallocForeignPtr, withForeignPtr)
import qualified Git
import Git.Libgit2 (LgRepo, getOid, repoObj)
import Path (Abs, Dir, Path, dirname, toFilePath)
import qualified System.FilePath as FP
import Text.Ginger.GVal (GVal, ToGVal, asBoolean, asHtml, asList, asLookup, asText, toGVal)
import Text.Ginger.Html (Html, html)
import Text.Ginger.Parse (SourcePos)
import Text.Ginger.Run (Run)

{-
Convenience type alias for the ginger run monad with git repo context.
-}
type RunRepo = Run SourcePos (ReaderT LgRepo IO) Html

unquote :: String -> String
unquote = init . tail

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode

{-
GVal implementation for a repository, accessed in the scope of an individual repository,
as well as in a list of all repositories in the index template.
-}
data Repo = Repo
    { repositoryPath :: Path Abs Dir
    , repositoryDescription :: Text
    , repositoryHead :: Maybe Commit
    }

instance ToGVal m (Path b t) where
    toGVal :: Path b t -> GVal m
    toGVal = toGVal . toFilePath

instance ToGVal m Repo where
    toGVal :: Repo -> GVal m
    toGVal repo =
        def
            { asHtml = html . pack . init . unquote . show . dirname . repositoryPath $ repo
            , asText = pack . show . dirname . repositoryPath $ repo
            , asLookup = Just . repoAsLookup $ repo
            }

repoAsLookup :: Repo -> Text -> Maybe (GVal m)
repoAsLookup repo = \case
    "name" -> Just . toGVal . init . unquote . show . dirname . repositoryPath $ repo
    "description" -> Just . toGVal . repositoryDescription $ repo
    "head" -> Just . toGVal . repositoryHead $ repo
    "updated" -> toGVal . show . Git.signatureWhen . Git.commitCommitter . commitGit <$> repositoryHead repo
    _ -> Nothing

{-
GVal implementation for commits, allowing them to be rendered in Ginger templates.
-}
data Commit = Commit
    { commitGit :: Git.Commit LgRepo
    , commitDiffs :: [Diff]
    }

instance ToGVal m Commit where
    toGVal :: Commit -> GVal m
    toGVal commit =
        def
            { asHtml = html . strip . T.takeWhile (/= '\n') . Git.commitLog . commitGit $ commit
            , asText = pack . show . Git.commitLog . commitGit $ commit
            , asLookup = Just . commitAsLookup $ commit
            }

commitAsLookup :: Commit -> Text -> Maybe (GVal m)
commitAsLookup commit = \case
    "id" -> Just . toGVal . show . untag . Git.commitOid . commitGit $ commit
    "href" -> Just . toGVal . (<> ".html") . show . untag . Git.commitOid . commitGit $ commit
    "title" -> Just . toGVal . strip . T.takeWhile (/= '\n') . Git.commitLog . commitGit $ commit
    "body" -> Just . toGVal . strip . T.dropWhile (/= '\n') . Git.commitLog . commitGit $ commit
    "message" -> Just . toGVal . strip . Git.commitLog . commitGit $ commit
    "author" -> Just . toGVal . strip . Git.signatureName . Git.commitAuthor . commitGit $ commit
    "committer" -> Just . toGVal . strip . Git.signatureName . Git.commitCommitter . commitGit $ commit
    "author_email" -> Just . toGVal . strip . Git.signatureEmail . Git.commitAuthor . commitGit $ commit
    "committer_email" -> Just . toGVal . strip . Git.signatureEmail . Git.commitCommitter . commitGit $ commit
    "authored" -> Just . toGVal . show . Git.signatureWhen . Git.commitAuthor . commitGit $ commit
    "committed" -> Just . toGVal . show . Git.signatureWhen . Git.commitCommitter . commitGit $ commit
    "encoding" -> Just . toGVal . strip . Git.commitEncoding . commitGit $ commit
    "parent" -> toGVal . show . untag <$> (listToMaybe . Git.commitParents . commitGit $ commit)
    "diffs" -> Just . toGVal . commitDiffs $ commit
    _ -> Nothing

{-
With `Diff`s there is a hierarchy:

- A commit has multiple diffs - one per file that changed.
- Each diff has 1 or more hunks
- Each hunk has a number of lines
-}
data Diff = Diff
    { diffNewFile :: Git.TreeFilePath
    , diffOldFile :: Maybe Git.TreeFilePath
    , diffStatus :: Delta
    , diffHunks :: [Hunk]
    }

data Delta
    = Unmodified
    | Added
    | Deleted
    | Modified
    | Renamed
    | Copied
    | Ignored
    | Untracked
    | Typechange
    deriving stock (Eq, Ord, Enum, Show)

type HunkHeader = ByteString
type DiffLine = ByteString

data Hunk = Hunk
    { hunkHeader :: HunkHeader
    , hunkLines :: [DiffLine]
    }

instance ToGVal m Diff where
    toGVal :: Diff -> GVal m
    toGVal diff =
        def
            { asHtml = html . bsToText . diffNewFile $ diff
            , asText = bsToText . diffNewFile $ diff
            , asLookup = Just . diffAsLookup $ diff
            }

diffAsLookup :: Diff -> Text -> Maybe (GVal m)
diffAsLookup diff = \case
    "new_file" -> Just . toGVal . diffNewFile $ diff
    "old_file" -> toGVal <$> diffOldFile diff
    "status" -> Just . toGVal . show . diffStatus $ diff
    "hunks" -> Just . toGVal . diffHunks $ diff
    _ -> Nothing

instance ToGVal m Hunk where
    toGVal :: Hunk -> GVal m
    toGVal hunk =
        def
            { asHtml = html . bsToText . hunkHeader $ hunk
            , asText = bsToText . hunkHeader $ hunk
            , asLookup = Just . hunkAsLookup $ hunk
            }

hunkAsLookup :: Hunk -> Text -> Maybe (GVal m)
hunkAsLookup hunk = \case
    "lines" -> Just . toGVal . fmap makeLine . hunkLines $ hunk
    "header" -> Just . toGVal . bsToText . hunkHeader $ hunk
    _ -> Nothing
  where
    makeLine :: DiffLine -> Line
    makeLine line =
        let text = bsToText line
         in Line text (cls . T.head $ text)

    cls :: Char -> String
    cls = \case
        '+' -> "add"
        '-' -> "sub"
        _ -> "def"

{-
Wrap diff lines when accessed so that they can each get a class string indicating
whether they are additions, subtractions, or context lines. They are primarily a
convenience for assigning CSS classes.
-}
data Line = Line
    { lineText :: Text
    , lineClass :: String
    }

instance ToGVal m Line where
    toGVal :: Line -> GVal m
    toGVal line =
        def
            { asHtml = html . lineText $ line
            , asText = lineText line
            , asLookup = Just . lineAsLookup $ line
            }

lineAsLookup :: Line -> Text -> Maybe (GVal m)
lineAsLookup line = \case
    "text" -> Just . toGVal . lineText $ line
    "class" -> Just . toGVal . lineClass $ line
    _ -> Nothing

{-
Next we have some data used to represent a repository's tree and the different kinds of
objects contained therein.
-}
data TreeFile = TreeFile
    { treeFilePath :: Text
    , treeFileContents :: TreeFileContents
    , treeFileMode :: TreeEntryMode
    }

data TreeFileContents = BinaryContents | FileContents ByteString | FolderContents [TreeFile]

data TreeEntryMode = ModeDirectory | ModePlain | ModeExecutable | ModeSymlink | ModeSubmodule
    deriving stock (Show)

{-
Some helper functions to convert from Haskell's LibGit2 BlobKind to our TreeEntryMode,
and then from that to Git's octal representation, as seen when calling `git ls-tree
<tree-ish>`
-}
blobkindToMode :: Git.BlobKind -> TreeEntryMode
blobkindToMode Git.PlainBlob = ModePlain
blobkindToMode Git.ExecutableBlob = ModeExecutable
blobkindToMode Git.SymlinkBlob = ModeSymlink

{-
This
-}
getBlobContents :: Git.BlobOid LgRepo -> ReaderT LgRepo IO TreeFileContents
getBlobContents oid = do
    repo <- Git.getRepository
    blobPtr <- liftIO mallocForeignPtr
    isBinary <- liftIO . withForeignPtr (repoObj repo) $ \repoPtr ->
        withForeignPtr blobPtr $ \blobPtr' ->
            withForeignPtr (getOid . untag $ oid) $ \oidPtr -> do
                r1 <- c'git_blob_lookup blobPtr' repoPtr oidPtr
                when (r1 < 0) $ throwM (Git.BackendError "Could not lookup blob")
                c'git_blob_is_binary =<< peek blobPtr'

    if toEnum . fromEnum $ isBinary -- This reads weird, but it goes CInt, to Int, to Bool.
        then return BinaryContents
        else FileContents <$> Git.catBlob oid

{-
GVal implementations for data definitions above, allowing commits to be rendered in
Ginger templates.
-}
instance ToGVal RunRepo TreeFile where
    toGVal :: TreeFile -> GVal RunRepo
    toGVal treefile =
        def
            { asHtml = html . treeFilePath $ treefile
            , asText = treeFilePath treefile
            , asLookup = Just . treeAsLookup $ treefile
            , asBoolean = True -- Used for conditionally checking readme/license template variables.
            }

instance ToGVal RunRepo TreeFileContents where
    toGVal :: TreeFileContents -> GVal RunRepo
    toGVal BinaryContents = def
    toGVal (FileContents bytestring) = toGVal bytestring
    toGVal (FolderContents treeFiles) =
        def
            { asHtml = html . pack . show . fmap treeFilePath $ treeFiles
            , asText = pack . show . fmap treeFilePath $ treeFiles
            , asList = Just . fmap toGVal $ treeFiles
            }

treeAsLookup :: TreeFile -> Text -> Maybe (GVal RunRepo)
treeAsLookup treefile = \case
    "path" -> Just . toGVal . treeFilePath $ treefile
    "name" -> Just . toGVal . FP.takeFileName . T.unpack . treeFilePath $ treefile
    "href" -> Just . toGVal . treePathToHref $ treefile
    "contents" -> Just . toGVal . treeFileContents $ treefile
    "tree" -> Just . toGVal . treeFileGetTree (treeFilePath treefile) . treeFileContents $ treefile
    "tree_recursive" -> Just . toGVal . treeFileGetTreeRecursive . treeFileContents $ treefile
    "mode" -> Just . toGVal . drop 4 . show . treeFileMode $ treefile
    "mode_octal" -> Just . toGVal . modeToOctal . treeFileMode $ treefile
    "mode_symbolic" -> Just . toGVal . modeToSymbolic . treeFileMode $ treefile
    "is_binary" -> Just . toGVal . treeFileIsBinary $ treefile
    "is_directory" -> Just . toGVal . treeFileIsDirectory $ treefile
    _ -> Nothing
  where
    treeFileGetTree :: Text -> TreeFileContents -> [TreeFile]
    treeFileGetTree parent (FolderContents fs) = filter atTop fs
      where
        atTop = notElem FP.pathSeparator . drop 1 . T.unpack . fromMaybe "" . T.stripPrefix parent . treeFilePath
    treeFileGetTree _ _ = []

    treeFileGetTreeRecursive :: TreeFileContents -> [TreeFile]
    treeFileGetTreeRecursive (FolderContents fs) = fs
    treeFileGetTreeRecursive _ = []

    treeFileIsBinary :: TreeFile -> Bool
    treeFileIsBinary treefile' = case treeFileContents treefile' of
        BinaryContents -> True
        _ -> False

    treeFileIsDirectory :: TreeFile -> Bool
    treeFileIsDirectory treefile' = case treeFileContents treefile' of
        FolderContents _ -> True
        _ -> False

    modeToOctal :: TreeEntryMode -> String
    modeToOctal ModeDirectory = "40000"
    modeToOctal ModePlain = "00644"
    modeToOctal ModeExecutable = "00755"
    modeToOctal ModeSymlink = "20000"
    modeToOctal ModeSubmodule = "60000"

    modeToSymbolic :: TreeEntryMode -> String
    modeToSymbolic ModeDirectory = "drwxr-xr-x"
    modeToSymbolic ModePlain = "-rw-r--r--"
    modeToSymbolic ModeExecutable = "-rwxr-xr-x"
    modeToSymbolic ModeSymlink = "l---------"
    modeToSymbolic ModeSubmodule = "git-module"

{-
Get the name of a tree file path's HTML file. Leading periods are dropped.
-}
treePathToHref :: TreeFile -> Text
treePathToHref = T.dropWhile (== '.') . flip T.append ".html" . T.replace "/" "." . treeFilePath

{-
Data to store information about references: tags and branches.
-}
data Ref = Ref
    { refName :: Git.RefName
    , refCommit :: Commit
    }

instance ToGVal RunRepo Ref where
    toGVal :: Ref -> GVal RunRepo
    toGVal ref =
        def
            { asHtml = html . refName $ ref
            , asText = refName ref
            , asLookup = Just . refAsLookup $ ref
            }

refAsLookup :: Ref -> Text -> Maybe (GVal RunRepo)
refAsLookup ref = \case
    "name" -> Just . toGVal . refName $ ref
    "commit" -> Just . toGVal . refCommit $ ref
    key -> commitAsLookup (refCommit ref) key
