#!/bin/sh
# usage: sh 19.sh FILENAME
set -eu

if [ $# -ne 1 ]; then
    echo "usage: sh 19.sh FILENAME"
    exit 1
elif [ ! -r "$1" ]; then
    echo "File not found."
    exit 1
fi

tail -n+3 "$1" | grep -cE "^($(head -n1 "$1" | sed 's/, /|/g'))+$"
