#!/usr/bin/env swipl
% usage: swipl 5.pl FILENAME
% https://adventofcode.com/2020/day/5

:- module(aoc2020day5, []).
:- module(aoc2020day5).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, IDs) :- once(phrase_from_file(load(IDs), Filename)).
load(IDs) --> sequence(seating_plan(0), IDs0), { sort(0, @>, IDs0, IDs) }.

seating_plan(P0, P) --> ( "F" | "L" ), { P1 is 2 * P0 },     seating_plan(P1, P).
seating_plan(P0, P) --> ( "B" | "R" ), { P1 is 2 * P0 + 1 }, seating_plan(P1, P).
seating_plan(P,  P) --> "\n".

part_one([Result | _], Result).

part_two([S1, S0 | Ss], Seat) :-
    ( succ(S0, S1) -> part_two([S0 | Ss], Seat) ; succ(Seat, S1) ).

main([File]) :-
    load(File, IDs),
    part_one(IDs, ResultOne),
    writeln(ResultOne),
    part_two(IDs, ResultTwo),
    writeln(ResultTwo).
