{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Config (getConfig, indexTemplate, commitTemplate, fileTemplate)
import Repositories (run)
import Templates (loadTemplates, loadTemplate)

main :: IO ()
main = do
    config <- getConfig "./config.dhall"
    templates <- loadTemplates config
    indexT <- loadTemplate $ indexTemplate config
    commitT <- loadTemplate $ commitTemplate config
    fileT <- loadTemplate $ fileTemplate config
    run config templates indexT commitT fileT
