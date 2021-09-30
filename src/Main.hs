{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Config (getConfig)
import Index (runIndex)
import Templates (loadTemplates)
import Repositories (run)

main :: IO ()
main = do
    env <- loadTemplates =<< getConfig "./config.dhall"
    runIndex env
    run env
