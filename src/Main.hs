{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Config (getConfig)
import Templates (loadTemplates)
import Repositories (run)

main :: IO ()
main = getConfig "./config.dhall" >>= loadTemplates >>= run
