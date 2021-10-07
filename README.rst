gitserve
========

``gitserve`` generates pretty web pages from your git repositories using custom
Jinja_\* HTML templates.

This was inspired by stagit_, which generates content with a hard-coded style,
so if you like that style and don't want to edit templates, you may prefer to
use that.

\*The templating engine is not Jinja per se, but ginger_, an implementation of
the Jinja language for Haskell. Please see their docs for any differences.

Why?
----

``gitserve`` complements your static site generator so that you can display
your code however you want, in a style consistent with the rest of your
website!

Contributing
------------

If you've come across a bug feel free to `open an issue`_.

Pull requests are also warmly welcome if you have any suggested improvements or
fixes.

Remaining tasks
---------------

- Commit diffs
- Write guide
- Specify config on the command line
- Improve performance

Meta
----

Development hosted @ https://github.com/m-col/gitserve.

Written by Matt Colligan - mcol@posteo.net

Distributed under the MIT license. See ``LICENSE`` for more information.

.. _Jinja: https://jinja.palletsprojects.com
.. _stagit: https://codemadness.org/git/stagit
.. _ginger: https://ginger.tobiasdammers.nl
.. _`open an issue`: https://github.com/m-col/gitserver/issues/new
