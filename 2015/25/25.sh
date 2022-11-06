#!/bin/sh
set -eu
APP="2015/25/25.com"

getcode() {
    actual=$($APP "$1" "$2")
    if [ $# -lt 3 ]; then
        set "$1" "$2" "$actual"
    fi
    printf "[%lu, %lu]\t  =\t\e[1m%lu\e[0m\n" "$1" "$2" "$3"
    if [ "$actual" != "$3" ]; then
        printf "\e[30mExpected \e[31m%s\e[30m to equal \e[33m%s\e[0m\n" "$actual" "$3"
        # exit 1
    fi
}

getcode 1 1 20151125
getcode 2 1 18749137
getcode 3 1 17289845
getcode 4 1 30943339
getcode 5 1 10071777
getcode 6 1 33511524
getcode 1 2 31916031
getcode 2 2 21629792
getcode 3 2 16929656
getcode 4 2 7726640
getcode 5 2 15514188
getcode 6 2 4041754
getcode 1 3 16080970
getcode 2 3 8057251
getcode 3 3 1601130
getcode 4 3 7981243
getcode 5 3 11661866
getcode 6 3 16474243
getcode 1 4 24592653
getcode 2 4 32451966
getcode 3 4 21345942
getcode 4 4 9380097
getcode 5 4 10600672
getcode 6 4 31527494
getcode 1 5 77061
getcode 2 5 17552253
getcode 3 5 28094349
getcode 4 5 6899651
getcode 5 5 9250759
getcode 6 5 31663883
getcode 1 6 33071741
getcode 2 6 6796745
getcode 3 6 25397450
getcode 4 6 24659492
getcode 5 6 1534922
getcode 6 6 27995004
echo
echo Attempt:
getcode 3029 2947
echo