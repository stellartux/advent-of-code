#!/usr/bin/env swipl
% usage: swipl 5.pl [FILENAME]

:- module(aoc2019day5, []).
:- module(aoc2019day5).
:- initialization(main, main).
:- use_module('../intcode.pl', [interpret/3, load//1]).

part_one(IntCode, Result) :-
    interpret([1], Output, IntCode),
    once(append(Zeroes, [Result], Output)),
    maplist(=(0), Zeroes).

part_two(IntCode, Result) :-
    interpret([5], Output, IntCode),
    once(append(Zeroes, [Result], Output)),
    maplist(=(0), Zeroes).

main([File]) :-
    once(phrase_from_file(load(IntCode), File)),
    part_one(IntCode, ResultOne),
    writeln(ResultOne),
    part_two(IntCode, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['2019/5/input.txt']).
