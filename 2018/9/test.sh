#!/bin/sh
APP="2018/9/9-1.com"

partone() {
    actual=$($APP "$1" "$2")
    if [ $# -lt 3 ]; then
        set "$1" "$2" "$actual"
    fi
    printf "%lu players; last marble is worth %lu points: high score is \e[1m%lu\e[0m\n" "$1" "$2" "$3"
    if [ "$actual" != "$3" ]; then
        printf "\e[30mExpected \e[31m%s\e[30m to equal \e[33m%s\e[0m\n" "$actual" "$3"
        # exit 1
    fi
}

partone 9 25 32
partone 10 1618 8317
partone 13 7999 146373
partone 17 1104 2764
partone 21 6111 54718
partone 30 5807 37305

partone 468 71843
partone 468 7184300

exit 0
