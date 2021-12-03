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
    { repos = folders
    , scan = False
    , template = "./template"
    , output = "./output"
    , host = "http://localhost:8000"
    }

-- If `scan` is True, then gitserve will look for git repositories in folders
-- nested within those listed in `repos`. Otherwise, the folders in `repos` are
-- assumed to be repositories themselves.

-- Note: The host is available verbatim in templates.
-- Port 8000 is appended here for easier testing with `python -m http.server`.

in config
