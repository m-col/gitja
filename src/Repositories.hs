{-# Language OverloadedStrings #-}  -- Needed for resolvReference

module Repositories (
    run
) where

import Control.Monad.IO.Class (liftIO)
import Data.Tagged
import Git
import Git.Libgit2 (lgFactory)

import Config (Config, repoPaths)

{-
This is the entrypoint that receives the ``Config`` and uses it to map over our
repositories, reading from them and writing out their web pages.
-}
run :: Config -> IO ()
run = mconcat . fmap processRepo . repoPaths

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
