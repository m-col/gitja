{-# Language OverloadedStrings #-}  -- Needed for repository paths.

module Repositories (
    getLog
) where

import Control.Monad.IO.Class (liftIO)
import Data.Tagged
import Git
import Git.Libgit2 (lgFactory)

getLog path = withRepository lgFactory path $ do
    maybeObjID <- resolveReference "HEAD"
    case maybeObjID of
        Just commitID -> do
            headCommit <- lookupCommit (Tagged commitID)
            liftIO $ print $ commitLog headCommit
        _ -> liftIO (print "Couldn't resolve HEAD")
