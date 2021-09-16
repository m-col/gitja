gitserve
========

This is all a work in progress and so nothing is implemented.

gitserve generates pretty web pages from your git repositories.

It has two modes of operations:

*Dynamic mode*: In this mode, gitserve serves generated web pages in response
to HTTP requests. Changes to the served git repositories are reflected
immediately as gitserve will generate new pages when needed.

*Static mode*: In this mode, gitserve will save generated web pages to file so
that other software can serve the output from the filesystem as static files.
Updates to the served git repositories are not reflected in the output until
gitserve is called again.


Outputs
-------

- Rendered markdown/rst readme (+ log in sidebar)
- File index + page per file in the tree
- Log index + page per commit
- Refs + tags index
- atom xml for repository
