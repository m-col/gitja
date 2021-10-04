gitserve
========

This is all a work in progress and so nothing is implemented.

gitserve generates pretty web pages from your git repositories using custom
HTML templates.

It has two modes of operations:

*Dynamic mode*: In this mode, gitserve serves generated web pages in response
to HTTP requests. Changes to the served git repositories are reflected
immediately as gitserve will generate new pages when needed.

*Static mode*: In this mode, gitserve will save generated web pages to file so
that other software can serve the output from the filesystem as static files.
Updates to the served git repositories are not reflected in the output until
gitserve is called again.


Template variables
------------------

There are two variable scopes. Only one is available in a given template:

- *Repository scope*: This exposes information that pertains to a single
  repository. Most templates are rendered once per configured repository.
- *Index scope*: This exposes information about the whole set of repositories.
  The index template is rendered once in total and uses this scope. This
  template is specified in the ``indexTemplate`` setting.


To do
-----

[x] Generate html file per commit with commit scope
[x] Generate html file per tree file with file scope
[ ] Expose readme url in repo scope for convenient hyperlinking
[ ] Expose license url in repo scope for convenient hyperlinking
[ ] Expose repo branches and tags in repo scope
[ ] Output something useful for git submodules
[ ] Generate RSS?
[ ] Write guide in readme
[ ] Remove targeted templates from general output
