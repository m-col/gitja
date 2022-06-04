-- This is an example configuration file for gitja.
-- It uses the Dhall configuration language: dhall-lang.org

-- This is a folder containing repositories.
let repos = ".."

-- And these are relative paths from 'repos' to those repositories.
let folders =
    [ "${repos}/gitja"
    ]

-- These are the configurable options that are used by gitja.
-- If copying, you may want to replace the relative paths for absolute paths.
let config =
    { repos = folders
    , scan = False
    , template = "./templates/docs"
    , output = "./output"
    , host = "http://localhost:8000"
    }

-- If `scan` is True, then gitja will look for git repositories in folders
-- nested within those listed in `repos`. Otherwise, the folders in `repos` are
-- assumed to be repositories themselves.

-- Note: The host is available verbatim in templates.
-- Port 8000 is appended here for easier testing with `python -m http.server`.

in config
