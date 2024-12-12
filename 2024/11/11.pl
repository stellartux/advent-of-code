#!/usr/bin/env swipl
% usage: swipl 11.pl [FILENAME]

:- module(aoc2024day11, []).
:- module(aoc2024day11).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(integer, " ", Result), eol, eos.

split_digits(N, N1, N2) :-
    number_chars(N, C),
    length(C, Length),
    divmod(Length, 2, L, 0),
    length(C1, L),
    once(append(C1, C2, C)),
    number_chars(N1, C1),
    number_chars(N2, C2).

blink([], []).
blink([0 | S0], [1 | S]) :- !, blink(S0, S).
blink([N | S0], [N1, N2 | S]) :- split_digits(N, N1, N2), !, blink(S0, S).
blink([N | S0], [N1 | S]) :- N1 is N * 2024, blink(S0, S).

foldn(0, _, R, R) :- !.
foldn(N, Fn, R0, R) :-
    succ(N0, N),
    call(Fn, R0, R1),
    foldn(N0, Fn, R1, R).

part_one(Input, Result) :-
    foldn(25, blink, Input, Blinked),
    length(Blinked, Result).

main([File]) :-
    phrase_from_file(load(Input), File),
    part_one(Input, ResultOne),
    writeln(ResultOne).

main([]) :- main(['input.txt']).
