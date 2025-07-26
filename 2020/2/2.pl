#!/usr/bin/env swipl
% usage: swipl 2.pl FILENAME
% https://adventofcode.com/2020/day/2

:- module(aoc2020day2, []).
:- module(aoc2020day2).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Lines) --> sequence(line, Lines), eol, eos.
line(t(Min, Max, Code, Codes)) -->
    integer(Min), "-", integer(Max), " ", [Code], ": ", nonblanks(Codes), eol.

is_valid_1(t(Min, Max, Code, Codes)) :-
    count_value(Codes, Code, 0, Count),
    between(Min, Max, Count).

count_value([], _, S, S).
count_value([X | Xs], Y, S0, S) :-
    ( X = Y -> succ(S0, S1) ; S0 = S1 ),
    count_value(Xs, Y, S1, S).

part_one --> include(is_valid_1), length.

is_valid_2(t(N1, N2, Code, Codes)) :-
    once((
        ( nth1(N1, Codes, Code), nth1(N2, Codes, OtherCode), OtherCode \= Code )
    ;   ( nth1(N1, Codes, OtherCode), nth1(N2, Codes, Code), OtherCode \= Code )
    )).

part_two --> include(is_valid_2), length.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
