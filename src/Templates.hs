{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Templates (
    Template (..),
    loadTemplate,
    generate,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as T
import Git.Libgit2 (LgRepo)
import Path (Abs, File, Path, Rel, filename, toFilePath)
import Path.IO (ignoringAbsence, makeRelativeToCurrentDir)
import System.Directory (removeFile)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import Text.Ginger.GVal (GVal)
import Text.Ginger.Html (Html, htmlSource)
import Text.Ginger.Parse (ParserError (..), SourcePos, parseGingerFile)
import Text.Ginger.Run (easyContext, runGingerT)
import qualified Hakyll as H
import Hakyll.Core.Logger as H
import Hakyll.Core.Runtime as H

import Types

data Template = Template
    { templatePath :: Path Rel File
    , templateHakyll :: H.Template
    }

{-
This tries to load a `Template` from the given file path.
-}
loadTemplate :: Path Abs File -> IO Template
loadTemplate path = do
    rel <- makeRelativeToCurrentDir path
    let template = H.readTemplate . toFilePath $ path
    return $ Template rel template

{-
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate ::
    Path Abs File ->
    Template ->
    H.Context String ->
    ReaderT LgRepo IO ()
generate output template context = liftIO $ do
    let output' = H.fromFilePath . toFilePath $ output
        rules = H.create [output'] do
            H.compile (H.applyTemplate (templateHakyll template) context =<< H.makeItem "")
    logger <- H.new H.Message
    H.run H.RunModeNormal conf logger rules
    return ()
    --liftIO . ignoringAbsence . removeFile $ output'
    --content <- liftIO . newIORef . TB.fromText $ ""

    --let emit :: Html -> ReaderT LgRepo IO ()
    --    emit = liftIO . modifyIORef' content . flip mappend . TB.fromText . htmlSource

    --runGingerT (easyContext emit context) . templateHakyll $ template
    --result <- liftIO . readIORef $ content
    --liftIO . T.writeFile output' . TB.toLazyText $ result

conf :: H.Configuration
conf = def
    { H.destinationDirectory = "output"
    , H.providerDirectory = "abc"
    }
