#!/usr/bin/env swipl
% usage: swipl 4.pl FILENAME
% https://adventofcode.com/2017/day/4

:- module(aoc2017day4, []).
:- module(aoc2017day4).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).

load(Lines) --> sequence(line, Lines).
line(Line)  --> sequence(nonblanks, " ", Line), "\n".

part_one(Lines, Result) :-
    aggregate_all(count, (
        member(Line, Lines),
        sort(Line, Sorted),
        same_length(Line, Sorted)
    ), Result).

part_two(Lines, Result) :-
    aggregate_all(count, (
        member(Line, Lines),
        maplist(msort, Line, Msorted),
        maplist(clumped, Msorted, Clumped),
        sort(Clumped, Sorted),
        same_length(Line, Sorted)
    ), Result).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
