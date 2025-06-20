#!/usr/bin/env swipl
% usage: swipl 9.pl [FILENAME]

:- module(aoc2019day9, []).
:- module(aoc2019day9).
:- initialization(main, main).
:- use_module('../intcode.pl', [interpret/3, load_zero_extended//1]).

part_one(M0, Result) :- interpret([1], [Result], M0).
part_two(M0, Result) :- interpret([2], [Result], M0).

main([File]) :-
    once(phrase_from_file(load_zero_extended(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    % 821,243,262 inferences, 52.768 CPU in 52.912 seconds (100% CPU, 15563284 Lips)
    time(part_two(Input, ResultTwo)),
    writeln(ResultTwo).

main([]) :- main(['2019/9/input.txt']).
