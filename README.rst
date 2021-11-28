gitserve
========

.. image:: https://img.shields.io/github/v/release/m-col/gitserve?color=904ff0
   :alt: GitHub release (latest SemVer)


``gitserve`` generates pretty web pages from your git repositories using custom
Jinja_\* HTML templates.

``gitserve`` complements your static site generator so that you can display
your code however you want, in a style consistent with the rest of your
website!

This was inspired by stagit_, which generates content with a hard-coded style,
so if you like that style and don't want to edit templates, you may prefer to
use that.

\*The templating engine is not Jinja per se, but ginger_, an implementation of
the Jinja language for Haskell. Please see their docs for any differences.

Getting Started
---------------

Currently installation is only possible via source::

    git clone https://github.com/m-col/gitserve
    cd gitserve
    stack install

Documentation and a preview of the kind of pages ``gitserve`` can generate is
available here_.

Contributing
------------

If you've come across a bug or have a question feel free to `open an issue`_.
Pull requests are also warmly welcome if you have any suggested improvements or
fixes. Please format code with fourmolu_'s defaults, use hlint_, and make sure
``test/test.sh`` is happy.

Meta
----

Development hosted @ https://github.com/m-col/gitserve. Written by Matt
Colligan and `licensed MIT <LICENSE>`_.

.. _Jinja: https://jinja.palletsprojects.com
.. _stagit: https://codemadness.org/git/stagit
.. _ginger: https://ginger.tobiasdammers.nl
.. _`open an issue`: https://github.com/m-col/gitserve/issues/new
.. _fourmolu: https://github.com/fourmolu/fourmolu
.. _hlint: https://github.com/ndmitchell/hlint
.. _here: https://gitserve.mcol.xyz
