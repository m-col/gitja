-- This is an example configuration file for gitserve.
-- It uses the Dhall configuration language: dhall-lang.org

-- This is a folder containing repositories.
let repos = ".."

-- And these are relative paths from 'repos' to those repositories.
let folders =
    [ "${repos}/gitserve"
    ]

-- These are the configurable options that are used by gitserve.
-- If copying, you may want to replace the relative paths for absolute paths.
let config =
    { repoPaths = folders
    , templateDirectory = "./templates"
    , indexTemplate = "./templates/index-main.html"
    , commitTemplate = "./templates/commit.html"
    , fileTemplate = "./templates/file.html"
    , outputDirectory = "./output"
    , host = "http://localhost"
    }

in config
