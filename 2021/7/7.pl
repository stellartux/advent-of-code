#!/usr/bin/env swipl
% usage: swipl 7.pl FILENAME

:- module(aoc2021day7, []).
:- module(aoc2021day7).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Crabs) --> sequence(integer, ",", Crabs), eol, eos.

part_one(Crabs, CrabClumps, MinSteps) :-
    length(Crabs, Length),
    N is Length div 2,
    nth1(N, Crabs, MedianCrab),
    aggregate_all(sum(Steps), (
        member(Crab - Clump, CrabClumps),
        Steps is Clump * abs(Crab - MedianCrab)
    ), MinSteps).

part_two(CrabClumps, MinTotalFuel) :-
    aggregate_all(min(TotalFuel), (
        member(Target - _, CrabClumps),
        aggregate_all(sum(Fuel), (
            member(Crab - Clump, CrabClumps),
            Steps is abs(Crab - Target),
            Fuel is Clump * Steps * (Steps + 1) div 2
        ), TotalFuel)
    ), MinTotalFuel).

main([File]) :-
    once(phrase_from_file(load(Crabs0), File)),
    msort(Crabs0, Crabs),
    clumped(Crabs, CrabClumps),
    part_one(Crabs, CrabClumps, ResultOne),
    writeln(ResultOne),
    part_two(CrabClumps, ResultTwo),
    writeln(ResultTwo).
