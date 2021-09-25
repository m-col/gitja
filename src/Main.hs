{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Config (getConfig)
import Repositories (run)
import Templates (loadTemplates, loadIndexTemplate)

main :: IO ()
main = do
    config <- getConfig "./config.dhall"
    templates <- loadTemplates config
    index <- loadIndexTemplate config
    run config templates index
