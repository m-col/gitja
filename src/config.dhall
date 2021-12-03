-- This is an example configuration file for gitserve.
-- It uses the Dhall configuration language: dhall-lang.org

-- You can list the git repositories here.
-- These will be used to generate HTML.
let repos = ".."

let folders =
    [ "${repos}/gitserve"
    ]

-- These are the configurable options that are used by gitserve.
-- You may want to replace the relative paths for absolute paths.
let config =
    { repoPaths = folders
    , scanRepoPaths = True
    , templateDirectory = "./template"
    , outputDirectory = "./output"
    , host = "http://localhost:8000"
    }

-- Note: The host is available verbatim in templates.
-- Port 8000 is appended here for easier testing with `python -m http.server`.

in config
