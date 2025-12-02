#!/usr/bin/env swipl
% usage: swipl 1.pl FILENAME
% https://adventofcode.com/2025/day/1

:- module(aoc2025day1, []).
:- module(aoc2025day1).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).

load(Result) --> sequence(line, Result).
line(N) --> "L", integer(N0), eol, { N is -N0 }.
line(N) --> "R", integer(N), eol.

process(N, (D0, R0), (D, R)) :-
    D is (D0 + N) mod 100,
    (   D == 0
    ->  R is R0 + 1
    ;   R = R0
    ).

part_one(Input, Result) :- foldl(process, Input, (50, 0), (_, Result)).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne).
