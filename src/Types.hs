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

import Data.ByteString.UTF8 (toString)
import Data.Default (def)
import Data.Tagged (untag)
import Data.Text (Text, pack, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Git
import Git.Libgit2 (LgRepo)
import System.FilePath (takeFileName)
import Text.Ginger.GVal (GVal, ToGVal, asBoolean, asHtml, asList, asLookup, asText, toGVal)
import Text.Ginger.Html (html)

{-
GVal implementation for a repository, accessed in the scope of an individual repository,
as well as in a list of all repositories in the index template.
-}
data Repo = Repo
    { repositoryPath :: FilePath
    , repositoryDescription :: Text
    , repositoryHead :: Maybe (Commit LgRepo)
    }

instance ToGVal m Repo where
    toGVal :: Repo -> GVal m
    toGVal repo =
        def
            { asHtml = html . pack . show . takeFileName . repositoryPath $ repo
            , asText = pack . show . takeFileName . repositoryPath $ repo
            , asLookup = Just . repoAsLookup $ repo
            }

repoAsLookup :: Repo -> Text -> Maybe (GVal m)
repoAsLookup repo = \case
    "name" -> Just . toGVal . takeFileName . repositoryPath $ repo
    "description" -> Just . toGVal . repositoryDescription $ repo
    "head" -> Just . toGVal . repositoryHead $ repo
    "updated" -> toGVal . show . signatureWhen . commitCommitter <$> repositoryHead repo
    _ -> Nothing

{-
GVal implementation for `Git.Commit r`, allowing commits to be rendered in Ginger
templates.
-}
instance ToGVal m (Commit LgRepo) where
    toGVal :: Commit LgRepo -> GVal m
    toGVal commit =
        def
            { asHtml = html . pack . show . commitLog $ commit
            , asText = pack . show . commitLog $ commit
            , asLookup = Just . commitAsLookup $ commit
            }

commitAsLookup :: Commit LgRepo -> Text -> Maybe (GVal m)
commitAsLookup commit = \case
    "id" -> Just . toGVal . show . untag . commitOid $ commit
    "title" -> Just . toGVal . strip . T.takeWhile (/= '\n') . commitLog $ commit
    "body" -> Just . toGVal . strip . T.dropWhile (/= '\n') . commitLog $ commit
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
Next we have some data used to represent a repository's tree and the different kinds of
objects contained therein.
-}
data TreeFile = TreeFile
    { treeFilePath :: TreeFilePath
    , treeFileContents :: TreeFileContents
    , treeFileMode :: TreeEntryMode
    }

data TreeFileContents = FileContents Text | FolderContents [TreeFile]

data TreeEntryMode = ModeDirectory | ModePlain | ModeExecutable | ModeSymlink | ModeSubmodule
    deriving stock (Show)

showMode :: TreeEntryMode -> String
showMode = drop 4 . show

{-
Some helper functions to convert from Haskell's LibGit2 BlobKind to our TreeEntryMode,
and then from that to Git's octal representation, as seen when calling `git ls-tree
<tree-ish>`
-}
blobkindToMode :: BlobKind -> TreeEntryMode
blobkindToMode PlainBlob = ModePlain
blobkindToMode ExecutableBlob = ModeExecutable
blobkindToMode SymlinkBlob = ModeSymlink

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
GVal implementations for data definitions above, allowing commits to be rendered in
Ginger templates.
-}
instance ToGVal m TreeFile where
    toGVal :: TreeFile -> GVal m
    toGVal treefile =
        def
            { asHtml = html . pack . toString . treeFilePath $ treefile
            , asText = pack . show . treeFilePath $ treefile
            , asLookup = Just . treeAsLookup $ treefile
            , asBoolean = True -- Used for conditionally checking readme/license template variables.
            }

instance ToGVal m TreeFileContents where
    toGVal :: TreeFileContents -> GVal m
    toGVal (FileContents text) = toGVal text
    toGVal (FolderContents treeFiles) =
        def
            { asHtml = html . pack . show . fmap (toString . treeFilePath) $ treeFiles
            , asText = pack . show . fmap (toString . treeFilePath) $ treeFiles
            , asList = Just . fmap toGVal $ treeFiles
            }

treeAsLookup :: TreeFile -> Text -> Maybe (GVal m)
treeAsLookup treefile = \case
    "path" -> Just . toGVal . treeFilePath $ treefile
    "href" -> Just . toGVal . treePathToHref . treeFilePath $ treefile
    "contents" -> Just . toGVal . treeFileContents $ treefile
    "mode" -> Just . toGVal . show . treeFileMode $ treefile
    "mode_octal" -> Just . toGVal . modeToOctal . treeFileMode $ treefile
    "mode_symbolic" -> Just . toGVal . modeToSymbolic . treeFileMode $ treefile
    "is_directory" -> Just . toGVal . treeFileIsDirectory $ treefile
    _ -> Nothing

treeFileIsDirectory :: TreeFile -> Bool
treeFileIsDirectory treefile = case treeFileContents treefile of
    FileContents _ -> False
    FolderContents _ -> True

{-
Get the name of a tree file path's HTML file.
-}
treePathToHref :: TreeFilePath -> Text
treePathToHref = flip T.append ".html" . T.replace "/" "." . decodeUtf8With lenientDecode

{-
Data to store information about references: tags and branches.
-}
data Ref = Ref
    { refName :: RefName
    , refCommit :: Commit LgRepo
    }

instance ToGVal m Ref where
    toGVal :: Ref -> GVal m
    toGVal ref =
        def
            { asHtml = html . refName $ ref
            , asText = refName ref
            , asLookup = Just . refAsLookup $ ref
            }

refAsLookup :: Ref -> Text -> Maybe (GVal m)
refAsLookup ref = \case
    "name" -> Just . toGVal . refName $ ref
    "commit" -> Just . toGVal . refCommit $ ref
    key -> commitAsLookup (refCommit ref) key
