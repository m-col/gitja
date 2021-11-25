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

declare -a TESTS
TESTS=(
    "gitserve/commit/0292014748caae952bbc8dd6225680d83c0a5135.html"
    "gitserve/file/test.templates.style.css.html"
)

rm -rf "$RESULT"
stack run -- -c test/config.dhall

for test in "${TESTS[@]}"
do
    diff --text --strip-trailing-cr --ignore-trailing-space \
	"$EXPECTED/$test" "$RESULT/$test" || {
	    let errors+=1
	    echo "^ ERRORED ON: $test"
	}
done

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