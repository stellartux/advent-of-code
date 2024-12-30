#!/usr/bin/env swipl
% usage: swipl 23.pl [FILENAME]

:- module(aoc2018day23, []).
:- module(aoc2018day23).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Nanobots) --> sequence(nanobot, Nanobots), eos.

nanobot(nanobot(pos(X, Y, Z), Range)) -->
    "pos=<", integer(X), ",", integer(Y), ",", integer(Z), ">, r=", integer(Range), eol.

manhattan_distance(pos(X1, Y1, Z1), pos(X2, Y2, Z2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).

part_one(Nanobots, Result) :-
    aggregate_all(
        max(Range, Position),
        member(nanobot(Position, Range), Nanobots),
        max(MaxRange, MaxPosition)),
    aggregate_all(count, (
        member(nanobot(Position, _), Nanobots),
        manhattan_distance(Position, MaxPosition, Distance),
        Distance =< MaxRange
    ), Result).

part_two(Nanobots, Result) :-
    Result = 0.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
