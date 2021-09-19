{-# Language OverloadedStrings #-}  -- Needed for resolveReference

module Repositories (
    run
) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldMap)
import Data.Tagged
import Data.Text (unpack, Text)
import Git
import Git.Types (RefTarget)
import Git.Libgit2 (lgFactory, LgRepo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import System.IO.Error (tryIOError)
import Text.Ginger (runGingerT, makeContextHtmlM, toGVal, GVal)
import Text.Ginger.Html (htmlSource)

import Config (Config, repoPaths, outputDirectory)
import Templates (Template, templateGinger, templatePath)

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
processRepo templates outputDirectory path = withRepository lgFactory path $ do
    liftIO $ createDirectoryIfMissing True outPath
    ref <- resolveReference "HEAD"
    case ref of
        Nothing -> liftIO . print $ "gitserve: " <> name <> ": Failed to resolve HEAD."
        Just commitID -> do
            description <- liftIO $ getDescription $ outPath </> "description"
            head <- lookupCommit (Tagged commitID)
            nodes <- lookupTree (commitTree head) >>= listTreeEntries
            return ()
  where
    name = takeFileName path
    outPath = outputDirectory </> name

    --mconcat $ runGingerT (makeContextHtmlM (scopeLookup context) (putStr . unpack . htmlSource)) tpl

getDescription :: FilePath -> IO (Maybe String)
getDescription path = tryIOError (readFile path) >>= \e ->
    case e of
        Right contents -> return $ Just contents
        Left err -> return Nothing
