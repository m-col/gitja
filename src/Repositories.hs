{-# Language OverloadedStrings #-}  -- Needed for repository paths.

module Repositories (
    run
) where

import Control.Monad.IO.Class (liftIO)
import Data.Tagged
import Git
import Git.Libgit2 (lgFactory)

import Config (Config, repoPaths)

getLog :: FilePath -> IO ()
getLog path = withRepository lgFactory path $ do
    maybeObjID <- resolveReference "HEAD"
    case maybeObjID of
        Just commitID -> do
            headCommit <- lookupCommit (Tagged commitID)
            liftIO $ print $ commitLog headCommit
        _ -> liftIO (print "Couldn't resolve HEAD")

run :: Config -> IO ()
run = mconcat . fmap getLog . repoPaths
