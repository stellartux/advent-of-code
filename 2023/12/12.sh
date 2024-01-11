#!/bin/sh
set -eu

file=${1:-2023/12/input.txt}
(
    cd "$(dirname "$file")"
    file=$(basename "$file")
    swipl 12.pl "$file"
    total=0
    mkdir -p tmp
    rm -rf tmp/**
    split -l 100 "$file" tmp/
    for input in tmp/**; do
        sed -i -E '
            s/[#.?]+/\0?\0?\0?\0?\0/g
            s/[0-9]+(,[0-9]+)*/\0,\0,\0,\0,\0/g' "$input"
        result=$(swipl 12.pl "$input")
        total=$((total + result))
    done
    echo $total
)
