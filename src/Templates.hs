{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates (
    Template (..),
    loadTemplate,
    generate,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as T
import Git.Libgit2 (LgRepo)
import Path (Abs, File, Path, Rel, filename, toFilePath)
import Path.IO (ignoringAbsence)
import System.Directory (removeFile)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.AST as G
import Text.Ginger.GVal (GVal)
import Text.Ginger.Html (Html, htmlSource)
import Text.Ginger.Parse (ParserError (..), SourcePos, parseGingerFile)
import Text.Ginger.Run (easyContext, runGingerT)

import Types

data Template = Template
    { templatePath :: Path Rel File
    , templateGinger :: G.Template SourcePos
    }

{-
This tries to load a `Template` from the given file path.
-}
loadTemplate :: Path Abs File -> IO (Maybe Template)
loadTemplate path =
    parseGingerFile includeResolver (toFilePath path) >>= \case
        Right parsed -> return . Just . Template (filename path) $ parsed
        Left err -> do
            informError (toFilePath path) err
            return Nothing
  where
    -- An attempt at pretty printing the error message.
    informError p (ParserError msg Nothing) =
        putStr $ "Template error: " <> p <> "\n" <> indent msg
    informError p (ParserError msg (Just pos)) =
        putStrLn $ "Template error: " <> p <> "\n" <> indent (show pos <> "\n" <> msg)
    indent = unlines . map (mappend "    ") . lines

    -- This resolves template 'includes'.
    includeResolver :: FilePath -> IO (Maybe String)
    includeResolver p = either (const Nothing) Just <$> tryIOError (readFile p)

{-
This is the generator function that receives repository-specific variables and uses
Ginger to render templates using them.
-}
generate ::
    Path Abs File ->
    HashMap.HashMap T.Text (GVal RunRepo) ->
    Template ->
    ReaderT LgRepo IO ()
generate output context template = do
    let output' = toFilePath output
    liftIO . ignoringAbsence . removeFile $ output'
    content <- liftIO . newIORef . TB.fromText $ ""

    let emit :: Html -> ReaderT LgRepo IO ()
        emit = liftIO . modifyIORef' content . flip mappend . TB.fromText . htmlSource

    runGingerT (easyContext emit context) . templateGinger $ template
    result <- liftIO . readIORef $ content
    liftIO . T.writeFile output' . TB.toLazyText $ result
