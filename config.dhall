-- This is an example configuration file for gitserve.
-- It uses the Dhall configuration language: dhall-lang.org

-- This is a folder containing repositories.
let root = ".."

-- And these are relative paths from root to those repositories.
let folders =
    [ "${root}/gitserve"
    ]

-- This is the configuration object that is used by gitserve
let config =
    { repoPaths = folders
    , templateDirectory = "./templates"
    , outputDirectory = "./output"
    , host = "http://localhost"
    }

in config
