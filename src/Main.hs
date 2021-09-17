{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config (getConfig)
import Repositories

main :: IO ()
main = do
    config <- getConfig "./config.dhall"
    print config
    getLog "."
