{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Config (getConfig)
import Repositories (run)
import Templates (loadTemplates)

main :: IO ()
main = do
    config <- getConfig "./config.dhall"
    templates <- loadTemplates config
    run config templates
