gitserve
========

``gitserve`` generates pretty web pages from your git repositories using custom
Jinja_\* HTML templates.

This was inspired by stagit_, which generates content with a hard-coded style,
so if you like that style and don't want to edit templates, you may prefer to
use that.

\* The templating engine is not Jinja_ per se, but ginger_, an implementation
of the Jinja language for Haskell.

Remaining tasks
---------------

- Commit diffs
- Write guide
- Specify config on the command line
- Improve performance

.. _Jinja: https://jinja.palletsprojects.com
.. _stagit: https://codemadness.org/git/stagit
.. _ginger: https://ginger.tobiasdammers.nl
