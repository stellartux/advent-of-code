#!/usr/bin/env sh
set -eu
APP=./23.com

assert() {
    if [ "$1" != "$2" ]; then
        printf "\e[30mExpected \e[31m%s\e[30m to equal \e[33m%s: %s\n" "$1" "$2" "$3"
        exit 1
    fi
}

(
    cd ../..
    make 2020/23/23.com
)
assert "$($APP -m 0 389125467)" 25467389 0
assert "$($APP -m 1 389125467)" 54673289 1
assert "$($APP -m 2 389125467)" 32546789 2
assert "$($APP -m 3 389125467)" 34672589 3
assert "$($APP -m 4 389125467)" 32584679 4
assert "$($APP -m 5 389125467)" 36792584 5
assert "$($APP -m 6 389125467)" 93672584 6
assert "$($APP -m 7 389125467)" 92583674 7
assert "$($APP -m 8 389125467)" 58392674 8
assert "$($APP -m 9 389125467)" 83926574 9
assert "$($APP -m 10 389125467)" 92658374 10
assert "$($APP 389125467)" 67384529 100
$APP 135468729
