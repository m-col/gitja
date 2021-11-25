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

if [[ $(basename "$PWD") != gitserve ]]
then
    echo "The test script should be run from the gitserve root project folder."
    exit 1
fi

rm -rf test/output
stack run -- -c test/config.dhall

let error=0

error() {
    echo Failed: $1
    echo Expected: $2
    echo But got: $3

    let error+=1
}

commit_file="test/output/gitserve/commit/0292014748caae952bbc8dd6225680d83c0a5135.html"
commit_result="$(cat $commit_file)"
commit_expected="0292014748caae952bbc8dd6225680d83c0a5135"

[[ "$commit_result" != "$commit_expected" ]] || {
    error "$commit_file" "$commit_expected" "$commit_result"
}

case "$error" in
    0)
	echo "All good!"
	exit 0
	;;
    *)
	echo "Had this many errors: $error"
	exit $error
	;;
esac
