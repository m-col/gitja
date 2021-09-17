{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config (getConfig)
import Repositories (run)

main :: IO ()
main = getConfig "./config.dhall" >>= run
