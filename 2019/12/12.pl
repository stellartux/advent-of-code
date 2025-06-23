#!/usr/bin/env swipl
% usage: swipl 12.pl FILENAME
% https://adventofcode.com/2019/day/12

:- module(aoc2019day12, []).
:- module(aoc2019day12).
:- initialization(main, main).
:- use_module(library(dcg/basics)).

format_(DCG) :- once(phrase(DCG, String)), format("~s\n", [String]).

load([moon(P1, V), moon(P2, V), moon(P3, V), moon(P4, V)]) -->
    { V = xyz(0, 0, 0) }, xyz(P1), eol, xyz(P2), eol, xyz(P3), eol, xyz(P4), eol, eol, eos.

xyz(xyz(PX, PY, PZ)) -->
    "<x=", integer(PX), ", y=", integer(PY), ", z=", integer(PZ), ">".

moon(moon(Position, Velocity)) -->
    "pos=", xyz(Position), ", vel=", xyz(Velocity), eol.

moons([A, B, C, D]) --> moon(A), moon(B), moon(C), moon(D).

moon_energy(moon(xyz(PX, PY, PZ), xyz(VX, VY, VZ)), Energy) :-
    Energy is (abs(PX) + abs(PY) + abs(PZ)) * (abs(VX) + abs(VY) + abs(VZ)).

system_energy(Moons, Energy) :-
    aggregate_all(sum(MoonEnergy), (
        member(Moon, Moons),
        moon_energy(Moon, MoonEnergy)
    ), Energy).

motion_step([
    moon(xyz(PX10, PY10, PZ10), xyz(VX10, VY10, VZ10)),
    moon(xyz(PX20, PY20, PZ20), xyz(VX20, VY20, VZ20)),
    moon(xyz(PX30, PY30, PZ30), xyz(VX30, VY30, VZ30)),
    moon(xyz(PX40, PY40, PZ40), xyz(VX40, VY40, VZ40))
],[
    moon(xyz(PX1 , PY1 , PZ1 ), xyz(VX1 , VY1 , VZ1 )),
    moon(xyz(PX2 , PY2 , PZ2 ), xyz(VX2 , VY2 , VZ2 )),
    moon(xyz(PX3 , PY3 , PZ3 ), xyz(VX3 , VY3 , VZ3 )),
    moon(xyz(PX4 , PY4 , PZ4 ), xyz(VX4 , VY4 , VZ4 ))
]) :-
    VX1 is VX10 + sign(PX20 - PX10) + sign(PX30 - PX10) + sign(PX40 - PX10),
    VX2 is VX20 + sign(PX10 - PX20) + sign(PX30 - PX20) + sign(PX40 - PX20),
    VX3 is VX30 + sign(PX10 - PX30) + sign(PX20 - PX30) + sign(PX40 - PX30),
    VX4 is VX40 + sign(PX10 - PX40) + sign(PX20 - PX40) + sign(PX30 - PX40),
    PX1 is PX10 + VX1,
    PX2 is PX20 + VX2,
    PX3 is PX30 + VX3,
    PX4 is PX40 + VX4,

    VY1 is VY10 + sign(PY20 - PY10) + sign(PY30 - PY10) + sign(PY40 - PY10),
    VY2 is VY20 + sign(PY10 - PY20) + sign(PY30 - PY20) + sign(PY40 - PY20),
    VY3 is VY30 + sign(PY10 - PY30) + sign(PY20 - PY30) + sign(PY40 - PY30),
    VY4 is VY40 + sign(PY10 - PY40) + sign(PY20 - PY40) + sign(PY30 - PY40),
    PY1 is PY10 + VY1,
    PY2 is PY20 + VY2,
    PY3 is PY30 + VY3,
    PY4 is PY40 + VY4,

    VZ1 is VZ10 + sign(PZ20 - PZ10) + sign(PZ30 - PZ10) + sign(PZ40 - PZ10),
    VZ2 is VZ20 + sign(PZ10 - PZ20) + sign(PZ30 - PZ20) + sign(PZ40 - PZ20),
    VZ3 is VZ30 + sign(PZ10 - PZ30) + sign(PZ20 - PZ30) + sign(PZ40 - PZ30),
    VZ4 is VZ40 + sign(PZ10 - PZ40) + sign(PZ20 - PZ40) + sign(PZ30 - PZ40),
    PZ1 is PZ10 + VZ1,
    PZ2 is PZ20 + VZ2,
    PZ3 is PZ30 + VZ3,
    PZ4 is PZ40 + VZ4.

fixate(Goal) --> Goal, ( { true } ; fixate(Goal) ).

part_one(Moons0, Result) :- part_one(1000, Moons0, Result).

part_one(N, Moons0, Result) :-
    call_nth(fixate(motion_step, Moons0, Moons), N),
    system_energy(Moons, Result).

part_two(Moons0, Result) :-
    call_nth(fixate(motion_step, Moons0, Moons), Result), Moons0 == Moons.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
