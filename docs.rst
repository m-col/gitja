=============
Documentation
=============

gitserve is a tool that reads from locally hosted or cloned git repositories
and generates static HTML pages using their data.


Usage
=====

gitserve is configured using a dhall_ config file. The `example config`_ is a
good place to start. It requires the following settings:

================= ==============================================
Setting           Description
================= ==============================================
repoPaths         A list of folders containing your git repositories.
templateDirectory The folder containing the <a href="#Templates">template</a>.
outputDirectory   Where to put the generated files.
host              The host URL, which is needed for creating links.
                  This can be a subdirectory.
================= ==============================================

Then, pass the config file to gitserve.

See the CLI help message for usage::

    Usage: gitserve [-c|--config CONFIG] [-q|--quiet] [-f|--force] [-v|--version]
      🐙 Templated web page generator for your git repositories

    Available options:
      -c,--config CONFIG       Configuration file to use (Default: ./config.dhall).
      -q,--quiet               Suppress non-error output.
      -f,--force               Force regeneration of all files.
      -v,--version             Print the gitserve's version.
      -h,--help                Show this help text

Note the "force" flag. By default, gitserve will not generate new output for
commits to save time. This flag will force regeneration of all files, which
would be needed if changes have been made to the template.


Templates
=========

The files that gitserve outputs are generated using ginger_ template files,
which use the Jinja_ templating language. If these are new to you, it may be
enough to skim through some of the examples in the templates_ folder, otherwise
the ginger_ docs can be very helpful to see what is supported.

Templates are a folder containing a number of ginger template files. There are
4 "scopes", each making available a unique set of variables storing information
about the git repositories. Each template file has access to a single one of
these scopes. The structure of the template folder determines the scopes of the
files contained therein.

To illustrate, this is the expected structure::

    templateDirectory/
        i_can_have_any_name.html
        and_there_can_be_any_number.html
        some_might_be_ginger_includes.html.include
        non_html_is_fine.css
        repo/
            inside_this_folder.html
            two_names_are_special.html
            file.html
            commit.html

The top-level folder, here templateDirectory, is that which is specified in the
config file.

Files ending in ".html" directly within that folder have access to the *index
scope*, and are each parsed exactly once and output into the output directory
with the same name. "include_" files are never copied but can be used to assist
in templating. They must be specified relative to the main template file ending
in ".html". The remaining files, and folders not called "repo" are copied as-is
as static files, such as CSS, images, etc.

The special folder "repo" has access to the *repo scope*, which exposes
information pertaining a single git repository. The template files contained
within this folder are parsed and output once per git repository.

The exceptions to this are the two special template files with the names
"file.html" and "commit.html". These have access to the *file scope* and
*commit scope* respectively, and are parsed and output once per file or commit.

The resulting folder structure found in the *outputDirectory* will look like
this (if the only specified git repository is gitserve)::

    outputDirectory/
        i_can_have_any_name.html
        and_there_can_be_any_number.html
        non_html_is_fine.css
        gitserve/
            inside_this_folder.html
            two_names_are_special.html
            file/
                LICENSE.html
                Makefile.html
                ...
            commit/
                0a18f38bb5c398bd192a6268281fc6abefaedd63.html
                0a7601059956d9c4d395f5d08e8cf48a515d080f.html
                ...


Template scopes
===============

The variables available within each scope are listed here for reference:

====== ============ ===========================================================
Scope  Variable     Description
====== ============ ===========================================================
Index  host         The string from the ``host`` config option.
       repositories A list of all of the git repositories.

Repo   -            In addition to the variables from the index scope...
       name         The repository name, taken from its folder name.
       description  A description taken from a file called "description" in
                    that folder.
       commits      A list of the repository's commits.
       tree         A list of the repository's files.
       tags         A list of the refs corresponding to tags.
       branches     A list of the refs corresponding to branches.
       readme       The repository's readme file, if it has one.
       license      The repository's license file, if it has one.

File   -            In addition to the variables from the Repo scope...
       file         A single file.
Commit -            In addition to the variables from the Repo scope...
       commit       A single commit.
====== ============ ===========================================================

As in Jinja_, a list can be accessed with indexing, and attributes can be
accessed using a dot notation. For example, a ``repository`` exposes an
attribute called "name", so to access the name of the first repository from
within the index scope you would do ``repositories[0].name``.

Here is the reference of attributes available on the variables that have them:

========== =============== ====================================================
Object     Attribute       Description
========== =============== ====================================================
repository name            The repository's name, taken from the folder name.
           description     A description taken from a file called "description"
                           in that folder.
           head            The current git commit.
           updated         The time when the current commit was committed.

commit     id              The SHA of the given commit.
           title           The commit message title.
           body            The commit message body.
           message         The entire message, including both title and body.
           author          The commit author.
           committer       The committer.
           author_email    The email address of the author.
           committer_email The email address of the committer.
           authored        The timestamp from when it was written.
           committed       The timestamp from when it was committed to this
                           branch.
           encoding        The commit encoding.
           parent          The SHA of the parent commit.

file       path            The path the file relative to the repository root.
           href            The HTML output path relative to outputDirectory.
           contents        The file's contents.
           mode            Directory, Plain, Executable, Symlink or Submodule.
           mode_octal      Mode in octal form e.g. "00644" for plain files.
           mode_symbolic   Mode in symbolic form e.g. ""-rw-r--r--" for plain
                           files.
           is_directory    A boolean, useful for ginger conditionals.

ref        name            The tag or branch name.
           commit          The commit pointed to by the tag or branch.
========== =============== ==========================================

Note that some attributes point to other objects that have attributes. For
example, ``branches[0].commit.parent`` will work as expected.

.. _dhall: https://dhall-lang.org
.. _`example config`: https://gitserve.mcol.xyz
.. _Jinja: https://jinja.palletsprojects.com
.. _ginger: https://ginger.tobiasdammers.nl
.. _templates: https://github.com/m-col/gitserve/tree/master/templates
.. _include: https://ginger.tobiasdammers.nl/guide/syntax/statements/#include
