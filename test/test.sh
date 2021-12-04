#!/usr/bin/env bash
: '

Testing gitserve
================

Running `make test` or `./test/test.sh` from the root project folder will run
this script, which tests gitserve.

Beside this file is the config file that gitserve uses. It runs using the
templates in this folder and outputs into this folder. Then the output is
compared with expected results.

Hopefully this will do until a "proper" test setup is needed.

'

set -u

if [[ $(basename "$PWD") != gitserve ]]
then
    echo "The test script should be run from the gitserve root project folder."
    exit 1
fi

let errors="0"
EXPECTED="test/expected"
RESULT="test/result"
rm -fr "$RESULT"

declare -a TESTS
TESTS=(
    "index.html"
    "link.html"  # Symbolic link at top level
    "style.css"  # Static file at top level
    "so_called_binary_file"
    "static/a_nice_file"  # Static folder at top level
    "gitserve/index.html"
    "gitserve/commit/0292014748caae952bbc8dd6225680d83c0a5135.html"
    "gitserve/file/test.expected.gitserve.file.html"
    "gitserve/file/test.templates.style.css.html"
    "gitserve/file/test.templates.so_called_binary_file.html"
    "gitserve/file/github.FUNDING.yml.html"  # Drops leading period
    "gitserve/log.html"  # Symbolic link inside repo/
    "gitserve/static/another_file"  # Static folder inside repo/
)

stack run -- -c test/config.dhall -q || exit 1

for test in "${TESTS[@]}"
do
    diff --text --strip-trailing-cr \
	"$EXPECTED/$test" "$RESULT/$test" || {
	    let errors+=1
	    echo "^ ERRORED ON: $test"
	}
done

test -f "$RESULT/title.html.include" && {
    let errors+=1
    echo "title.html.include found in results"
}

case "$errors" in
    0)
	echo "All good!"
	;;
    *)
	echo "Had this many errors: $errors"
	;;
esac

rm -r "$RESULT"
exit $errors
