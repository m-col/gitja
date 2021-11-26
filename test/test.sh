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

exit 0

set -ue

if [[ $(basename "$PWD") != gitserve ]]
then
    echo "The test script should be run from the gitserve root project folder."
    exit 1
fi

let errors="0" "1"
EXPECTED="test/expected"
RESULT="test/result"

declare -a TESTS
TESTS=(
    "index.html"
    "style.css"
    "static/a_nice_file"
    "gitserve/index.html"
    "gitserve/commit/0292014748caae952bbc8dd6225680d83c0a5135.html"
    "gitserve/file/test.templates.html"
    "gitserve/file/test.templates.style.css.html"
)

rm -rf "$RESULT"
stack run -- -c test/config.dhall -q

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
	exit 0
	;;
    *)
	echo "Had this many errors: $errors"
	exit $errors
	;;
esac
