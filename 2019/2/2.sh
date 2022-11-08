#!/usr/bin/env bash
set -euo pipefail

if [ "$1" -eq 1 ]; then
    ./2019/2/2.com ./2019/2/input.txt 12 2
else
    for i in {0..99}; do
        for j in {0..99}; do
            x=$(./2019/2/2.com ./2019/2/input.txt "$i" "$j")
            if [ "$x" -eq 19690720 ]; then
                printf "%d%d\n" "$i" "$j"
                break
            fi
        done
    done
fi

exit 0
