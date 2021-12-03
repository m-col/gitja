let config =
    { repoPaths = ["."]
    , scanRepoPaths = False
    , templateDirectory = "./test/templates"
    , outputDirectory = "./test/result"
    , host = "https://github.com/m-col/gitserve"
    }

in config
