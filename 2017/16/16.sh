#!/bin/sh
# usage: sh 16.sh FILENAME
# converts a data file to the equivalent C program
# see Makefile for use
set -eu

LENGTH=$(grep -Po '\d+' "$1" | awk '
    $1 > t { t = $1 }
    END { print t + 1 }
')

STRING=$(echo "$LENGTH" | awk '{ print substr("abcdefghijklmnopqrstuvwxyz", 1, $1) }' )

echo "
#include <stdio.h>
#include <string.h>
#define LENGTH $LENGTH
#define COUNT 1000000000

char s[LENGTH] = \"$STRING\";

void swap(int x) {
  char b[LENGTH];
  strncpy(b, s, LENGTH);
  strncpy(s, b + LENGTH - x, x);
  strncpy(s + x, b, LENGTH - x);
}

void exchange(int a, int b) {
  char t = s[a];
  s[a] = s[b];
  s[b] = t;
}

int indexof(char c) {
  int i = 0;
  for (; i < LENGTH; ++i) {
    if (s[i] == c) return i;
  }
  return -1;
}

void partner(char a, char b) {
  exchange(indexof(a), indexof(b));
}

void dance() {"

<"$1" tr , \\n | sed -E "
s/s(.*)/    swap\(\1);/g
s/x(.*)\/(.*)/    exchange(\1, \2);/g
s/p(.*)\/(.*)/    partner('\1', '\2');/g
"

echo "}

int main() {
  dance();
  puts(s);
  for (int i = 1; i < COUNT; ++i) {
    dance();
    if (!strcmp(s, \"$STRING\")) i = COUNT - 1 - COUNT % (i + 1);
  }
  puts(s);
}"
