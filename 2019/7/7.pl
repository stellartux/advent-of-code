#!/usr/bin/env swipl
% usage: swipl 7.pl [FILENAME]

:- module(aoc2019day7, []).
:- module(aoc2019day7).
:- initialization(main, main).
:- use_module('../intcode.pl', [interpret/3, load//1]).

part_one(VM, Result) :-
    numlist(0, 4, Nums),
    aggregate_all(max(Signal), (
        permutation(Nums, PhaseSettings),
        part_one(PhaseSettings, VM, Signal)
    ), Result).

part_one([A, B, C, D, E], VM, Signal) :-
    interpret([A, 0],     OutA, VM),
    interpret([B | OutA], OutB, VM),
    interpret([C | OutB], OutC, VM),
    interpret([D | OutC], OutD, VM),
    interpret([E | OutD], [Signal], VM).

part_two(VM, Result) :-
    numlist(5, 9, Nums),
    aggregate_all(max(Signal), (
        permutation(Nums, PhaseSettings),
        part_two(PhaseSettings, VM, Signal)
    ), Result).

part_two([A, B, C, D, E], VM0, Signal) :-
    interpret([A, 0 | OutE0], OutA, VM0),
    interpret([B | OutA], OutB, VM0),
    interpret([C | OutB], OutC, VM0),
    interpret([D | OutC], OutD, VM0),
    interpret([E | OutD], OutE, VM0),
    once(append(OutE0, [Signal], OutE)).

main([File]) :-
    once(phrase_from_file(load(VM), File)),
    part_one(VM, ResultOne),
    writeln(ResultOne),
    % 23,351,435 inferences, 1.463 CPU in 1.466 seconds (100% CPU, 15956216 Lips)
    time(part_two(VM, ResultTwo)),
    writeln(ResultTwo).

main([]) :- main(['2019/7/input.txt']).
