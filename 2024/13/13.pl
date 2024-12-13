#!/usr/bin/env swipl
% usage: swipl 13.pl [FILENAME]

:- module(aoc2024day13, []).
:- module(aoc2024day13).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(claw_machine, Result), eos.

claw_machine(machine(AX, AY, BX, BY, PX, PY)) -->
    "Button A: X+", integer(AX), ", Y+", integer(AY), eol,
    "Button B: X+", integer(BX), ", Y+", integer(BY), eol,
    "Prize: X=", integer(PX), ", Y=", integer(PY), eol, eol.

solve(machine(AX, AY, BX, BY, PX, PY), Cost) :-
    A in 1..100,
    B in 1..100,
    PX #= A * AX + B * BX,
    PY #= A * AY + B * BY,
    Cost #= 3 * A + B.

part_one(Input, Result) :-
    aggregate_all(sum(Cost), (
        member(Machine, Input),
        solve(Machine, Cost)
    ), Result).

solve2(machine(AX, AY, BX, BY, PX, PY), Cost) :-
    A #> 0,
    B #> 0,
    PX #= A * AX + B * BX,
    PY #= A * AY + B * BY,
    Cost #= 3 * A + B.

part_two(Input, Result) :-
    aggregate_all(sum(Cost), (
        member(machine(AX, AY, BX, BY, PX0, PY0), Input),
        PX is PX0 + 10000000000000,
        PY is PY0 + 10000000000000,
        solve2(machine(AX, AY, BX, BY, PX, PY), Cost)
    ), Result).

main([File]) :-
    phrase_from_file(load(Input), File),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
