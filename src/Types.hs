{-# Language DerivingStrategies #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}  -- Needed for `instance ToGVal`
{-# Language MultiParamTypeClasses #-}  -- Needed for `instance ToGVal`
{-# Language InstanceSigs #-}  -- Needed for toGVal type signature

module Types where

import Data.Default (def)
import Data.Tagged (untag)
import Data.Text (pack, Text, strip)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Git
import Git.Libgit2 (LgRepo)
import Text.Ginger.GVal (GVal, toGVal, ToGVal, asText, asHtml, asLookup, asList)
import Text.Ginger.Html (html)
import qualified Data.Text as T

{-
GVal implementation for `Git.Commit r`, allowing commits to be rendered in Ginger
templates.
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

data TreeFileContents = FileContents Text | FolderContents [TreeFilePath]

data TreeEntryMode = ModeDirectory | ModePlain | ModeExecutable | ModeSymlink | ModeSubmodule
    deriving stock Show

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
modeToOctal ModeDirectory  = "40000"
modeToOctal ModePlain      = "00644"
modeToOctal ModeExecutable = "00755"
modeToOctal ModeSymlink    = "20000"
modeToOctal ModeSubmodule  = "60000"

modeToSymbolic :: TreeEntryMode -> String
modeToSymbolic ModeDirectory  = "drwxr-xr-x"
modeToSymbolic ModePlain      = "-rw-r--r--"
modeToSymbolic ModeExecutable = "-rwxr-xr-x"
modeToSymbolic ModeSymlink    = "l---------"
modeToSymbolic ModeSubmodule  = "git-module"

{-
GVal implementations for data definitions above, allowing commits to be rendered in
Ginger templates.
-}
instance ToGVal m TreeFile where
    toGVal :: TreeFile -> GVal m
    toGVal treefile = def
        { asHtml = html . pack . show . treeFilePath $ treefile
        , asText = pack . show . treeFilePath $ treefile
        , asLookup = Just . treeAsLookup $ treefile
        }

instance ToGVal m TreeFileContents where
    toGVal :: TreeFileContents -> GVal m
    toGVal (FileContents text) = toGVal text
    toGVal (FolderContents filePaths) = def
        { asHtml = html . pack . show $ filePaths
        , asText = pack . show $ filePaths
        , asList = Just . fmap toGVal $ filePaths
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
treePathToHref = flip T.append ".html" . T.replace "/" "." .  decodeUtf8With lenientDecode
