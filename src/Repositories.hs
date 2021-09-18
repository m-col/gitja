{-# Language OverloadedStrings #-}  -- Needed for resolvReference

module Repositories (
    run
) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldMap)
import Data.Tagged
import Git
import Git.Libgit2 (lgFactory)

import Config (Config, repoPaths)
import Templates (Template, templatePath, templateContents)

{-
This is the entrypoint that receives the ``Config`` and uses it to map over our
repositories, reading from them and writing out their web pages.
-}
run :: Config -> [Template] -> IO ()
run config templates = foldMap processRepo . repoPaths $ config

----------------------------------------------------------------------------------------

{-
This receives a file path to a single repository and tries to process it. If the
repository doesn't exist or is unreadable in any way we can forget about it and move on
(after informing the user of course).
-}
processRepo :: FilePath -> IO ()
processRepo path = withRepository lgFactory path $ do
    maybeObjID <- resolveReference "HEAD"
    case maybeObjID of
        Just commitID -> do
            headCommit <- lookupCommit (Tagged commitID)
            liftIO $ print $ commitLog headCommit
        _ -> liftIO (print "Couldn't resolve HEAD")


-- Variables:
title = "gitserve"
description = ""
host = "http://localhost"
path = ""
