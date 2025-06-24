#!/usr/bin/env swipl
% usage: swipl 12.pl FILENAME
% https://adventofcode.com/2019/day/12

:- module(aoc2019day12, []).
:- module(aoc2019day12).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

load(Systems) -->
    xyz(P1), eol, xyz(P2), eol, xyz(P3), eol, xyz(P4), eol, eol, eos,
    { transpose([P1, P2, P3, P4], Systems) }.

xyz([PX, PY, PZ]) -->
    "<x=", integer(PX), ", y=", integer(PY), ", z=", integer(PZ), ">".

fixate(Goal) --> Goal, ( {} ; fixate(Goal) ).

step(
    [V10, V20, V30, V40, P10, P20, P30, P40],
    [V1,  V2,  V3,  V4,  P1,  P2,  P3,  P4 ]
) :-
    V1 is V10 + sign(P20 - P10) + sign(P30 - P10) + sign(P40 - P10),
    V2 is V20 + sign(P10 - P20) + sign(P30 - P20) + sign(P40 - P20),
    V3 is V30 + sign(P10 - P30) + sign(P20 - P30) + sign(P40 - P30),
    V4 is V40 + sign(P10 - P40) + sign(P20 - P40) + sign(P30 - P40),
    P1 is P10 + V1,
    P2 is P20 + V2,
    P3 is P30 + V3,
    P4 is P40 + V4.

system_energy([
    [VX1, VX2, VX3, VX4, PX1, PX2, PX3, PX4],
    [VY1, VY2, VY3, VY4, PY1, PY2, PY3, PY4],
    [VZ1, VZ2, VZ3, VZ4, PZ1, PZ2, PZ3, PZ4]
], Energy) :-
    Energy is (abs(PX1) + abs(PY1) + abs(PZ1)) * (abs(VX1) + abs(VY1) + abs(VZ1))
            + (abs(PX2) + abs(PY2) + abs(PZ2)) * (abs(VX2) + abs(VY2) + abs(VZ2))
            + (abs(PX3) + abs(PY3) + abs(PZ3)) * (abs(VX3) + abs(VY3) + abs(VZ3))
            + (abs(PX4) + abs(PY4) + abs(PZ4)) * (abs(VX4) + abs(VY4) + abs(VZ4)).

run_system(N, S0, S) :- call_nth(fixate(step, [0, 0, 0, 0 | S0], S), N).

part_one    --> part_one(1000).
part_one(N) --> maplist(run_system(N)), system_energy.

loop_length(System0, LoopLength) :-
    System1 = [0, 0, 0, 0 | System0],
    call_nth(fixate(step, System1, System), LoopLength),
    System1 == System,
    !.

part_two(Systems, LoopLength) :-
    findall(Length, (member(System, Systems), loop_length(System, Length)), Lengths),
    foldl([A, B, C] >> (C is lcm(A, B)), Lengths, 1, LoopLength).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
