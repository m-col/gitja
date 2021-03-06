Documentation
=============

`gitja` is a tool that reads from locally hosted or cloned git repositories and
generates static HTML pages using their data and
[Jinja](https://jinja.palletsprojects.com) templates.

Usage
-----

### Configuration file

gitja is configured using a [dhall](https://dhall-lang.org) config file. The
[example config](https://github.com/m-col/gitja/blob/master/config.dhall) is a
good place to start, or alternatively create one in the current folder by
running `gitja -t`.

It requires the following settings:

| Setting    | Description                                                     |
| ---------- | --------------------------------------------------------------- |
| `repos`    | A list of folders containing your git repositories.             |
| `scan`     | Whether `repos` lists repos or folders containing nested repos. |
| `template` | The folder containing the template (see below).                 |
| `output`   | Where to put the generated files.                               |
| `host`     | The host URL, which is needed for creating links.               |

If `scan` is `True`, then gitja will look for git repositories in folders
nested within those listed in `repos`. Otherwise, the folders in `repos` are
assumed to be repositories themselves.

Then, pass the config file to gitja.

### CLI

See the CLI help message for usage:

    Usage: gitja [-c|--config CONFIG] [-q|--quiet] [-f|--force] [-t|--template]
                    [-v|--version]
      🐙 Templated web page generator for your git repositories

    Available options:
      -c,--config CONFIG       Configuration file to use (Default: ./config.dhall).
      -q,--quiet               Suppress non-error output.
      -f,--force               Force regeneration of all files.
      -t,--template            Create a template and config in the current folder.
      -v,--version             Print the gitja's version.
      -h,--help                Show this help text

Note the `force` flag. By default, gitja will not generate new output for
commits to save time. This flag will force regeneration of all files, which
would be needed if changes have been made to the template.

Templates
---------

A base template can be created in the current folder by running `gitja -t`.
This can be used as a starting point to creating your own template.

### Folder structure

The files that gitja outputs are generated using
[ginger](https://ginger.tobiasdammers.nl) template files, which use the
[Jinja](https://jinja.palletsprojects.com) templating language. If these are
new to you, it may be enough to skim through some of the examples in the
[templates](https://github.com/m-col/gitja/tree/master/templates) folder,
otherwise the ginger docs can be very helpful to see what is supported.

Templates are a folder containing a number of ginger template files.  There are
4 "scopes", each making available a unique set of variables storing information
about the git repositories. Each template file has access to a single one of
these scopes. The structure of the template folder determines the scopes of the
files contained therein.

To illustrate, this is the expected structure:

    template/
        i_can_have_any_name.html
        and_there_can_be_any_number.html
        some_might_be_ginger_includes.html.include
        non_html_is_fine.css
        repo/
            inside_this_folder.html
            two_names_are_special.html
            file.html
            commit.html

The top-level folder, here `template`, is that which is specified in the config
file.

Files ending in ".html" directly within that folder have access to the *index
scope*, and are each parsed exactly once and output into the output directory
with the same name.
"[include](https://ginger.tobiasdammers.nl/guide/syntax/statements/#include)"
files are never copied but can be used to assist in templating.

The special folder "repo" has access to the *repo scope*, which exposes
information pertaining to a single git repository. The template files contained
within this folder are parsed and output once per git repository.

The exceptions to this are the two special template files with the names
"file.html" and "commit.html". These have access to the *file scope* and
*commit scope* respectively, and are parsed and output once per file or commit.

The resulting folder structure found in `output` will look like this (if
`repos` only contains gitja):

    output/
        i_can_have_any_name.html
        and_there_can_be_any_number.html
        non_html_is_fine.css
        gitja/
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
        ...

### Static files

Files in the template folder that do not end in ".html" or ".include" or are
symbolic links, as well as directories that are not called "repo", are
considered static content. These are copied unchanged (i.e. symbolic links are
copied as symbolic links) to the output folder. This is useful for putting CSS,
images etc into the output.

Similarly, any static content found within the template's "repo/" folder is
copied unchanged into the output folder for every repository.

### Scopes

The variables available within each scope are listed here for reference:

| Scope  | Variable        | Description                                           |
| ------ | --------------- | ----------------------------------------------------- |
| Index  | host            | The string from the `host` config option.             |
|        | repositories    | A list of all of the git repositories.                |
| Repo   |                 | *In addition to the variables from the index scope...*|
|        | name            | The repository name, taken from its folder name.      |
|        | description     | The repository's description (see below).             |
|        | commits         | A list of the repository's commits.                   |
|        | tree            | A list of the top-level folder's contents.            |
|        | tree\_recursive | A list of *all* of the repository's contents.         |
|        | tags            | A list of the refs corresponding to tags.             |
|        | branches        | A list of the refs corresponding to branches.         |
|        | readme          | The repository's readme file, if it has one.          |
|        | license         | The repository's license file, if it has one.         |
| File   |                 | *In addition to the variables from the Repo scope...* |
|        | file            | A single file.                                        |
| Commit |                 | *In addition to the variables from the Repo scope...* |
|        | commit          | A single commit.                                      |

As in [Jinja](https://jinja.palletsprojects.com), a list can be accessed with
indexing, and attributes can be accessed using a dot notation. For example, a
`repository` exposes an attribute called "name", so to access the name of the
first repository from within the index scope you would do
`repositories[0].name`.

Here is the reference of attributes available on the variables that have them:

| Object     | Attribute        | Description                                              |
| ---------- | ---------------- | -------------------------------------------------------- |
| repository | name             | The repository's name, taken from the folder name.       |
|            | description      | The repository's description (see below).                |
|            | head             | The current git commit.                                  |
|            | updated          | The time when the current commit was committed.          |
| file       | path             | The path the file relative to the repository root.       |
|            | name             | The name of the file.                                    |
|            | href             | The name of the HTML file for this file.                 |
|            | contents         | The file's contents.                                     |
|            | mode             | Directory, Plain, Executable, Symlink or Submodule.      |
|            | mode\_octal      | Mode in octal form e.g. "00644" for plain files.         |
|            | mode\_symbolic   | Mode in symbolic form e.g. ""-rw-r--r--" for plain files.|
|            | is\_directory    | A boolean, useful for ginger conditionals.               |
|            | is\_binary       | A boolean, tells you if the contents can be rendered.    |
|            | tree             | A list of a directory's direct contents.                 |
|            | tree\_recursive  | A list of *all* of a directory's contents.               |
| ref        | name             | The tag or branch name.                                  |
|            | commit           | The commit pointed to by the tag or branch.              |
| commit     | id               | The SHA of the given commit.                             |
|            | href             | The name of the HTML file for this commit.               |
|            | title            | The commit message title.                                |
|            | body             | The commit message body.                                 |
|            | message          | The entire message, including both title and body.       |
|            | diff             | The list of diff objects for this commit.                |
|            | author           | The commit author.                                       |
|            | committer        | The committer.                                           |
|            | author\_email    | The email address of the author.                         |
|            | committer\_email | The email address of the committer.                      |
|            | authored         | The timestamp from when it was written.                  |
|            | committed        | The timestamp from when it was committed to this branch. |
|            | encoding         | The commit encoding.                                     |
|            | parent           | The SHA of the parent commit.                            |
| diff       | new\_file        | The name of the file after the diff.                     |
|            | old\_file        | The name of the file before the diff.                    |
|            | status           | The type of the change (see below).                      |
|            | hunks            | The list of modified hunks for this file.                |
| hunk       | header           | The hunk's header e.g. "@@ -2,44 +2,22 @@".              |
|            | lines            | The list of lines  that form this hunk.                  |
| line       | text             | The line, possibly prefixed by `+` or `-`.               |
|            | class            | `"add"`, `"sub"` or `"def"` -- useful for CSS.           |

Note:

- Some attributes point to other objects that have attributes. For example,
  `branches[0].commit.parent` will work as expected.
- "file" includes directories and symbolic links.

### Diffs

Each commit has a corresponding list of diffs. Each diff corresponds to the
changes made to a single file. A diff's `new_file` and `old_file` will differ
only if the file was renamed. The `status` can be one of: Unmodified, Added,
Deleted, Modified, Renamed, Copied, Ignored, Untracked, TypeChange.

A diff contains a series of 'hunks' that represent contiguous blocks of text
within the file that were modified in some way. Each has a header that
indicates where the hunk is in the file. The hunks each also expose a list of
_lines_, each of which exposes the text content of that line, as well as a
'class'. The class is a convenience attribute that can be used to set the CSS
class of the line without needing to parse the line text from within a template
file. This makes it easier to style additions and subtractions from other (def
for default) lines.

### Descriptions

Each repository can have a description that is available in the templates. This
is how the description for a given respository is determined:

1. Look for a file at `repo/description`, and if found read from there.
2. Otherwise, look for a file at `repo/.git/description`, and if found read from there.
2. Otherwise, simply use the repository folder's name.

Questions?
----------

If any of this is unclear or could be improved, please do feel free to post an
issue on GitHub (or contact me another way). Contributions of code and docs are
also warmly welcome.
