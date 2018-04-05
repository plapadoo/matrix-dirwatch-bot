#!/usr/bin/env bash

if ! command -v unbuffer >/dev/null; then
    echo "you need the unbuffer command from the expect package for this to work"
    exit -1
fi

if ! command -v curl >/dev/null; then
    echo "you need the curl for this to work"
    exit -1
fi

unbuffer result/bin/matrix-dirwatch-exe "--directory=$1" "--exclude=.tmp" | xargs -I foobar curl -X POST --data-binary foobar 'http://localhost:1339/!UzoskWNmedIeiGZjPB:chat.plapadoo.de'
