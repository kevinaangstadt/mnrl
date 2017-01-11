#!/bin/bash

#$1 file 1
#$2 file 2

"${BASH_SOURCE%/*}"/jq/jq --argfile a $1 --argfile b $2 -n 'def post_recurse(f): def r: (f | select(. != null) | r), .; r; def post_recurse: post_recurse(.[]?); ($a | (post_recurse | arrays) |= sort) as $a | ($b | (post_recurse | arrays) |= sort) as $b | $a == $b'