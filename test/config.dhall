let config =
    { repos = ["."]
    , scan = False
    , template = "./test/templates"
    , output = "./test/result"
    , host = "https://github.com/m-col/gitserve"
    }

in config
