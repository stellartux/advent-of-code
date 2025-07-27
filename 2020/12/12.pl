#!/usr/bin/env swipl
% usage: swipl 12.pl FILENAME
% https://adventofcode.com/2020/day/12

:- module(aoc2020day12, []).
:- module(aoc2020day12).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).
load(Instructions) --> sequence(instruction, Instructions).

instruction(north(N))     --> "N", integer(N), eol.
instruction(east(N))      --> "E", integer(N), eol.
instruction(south(N))     --> "S", integer(N), eol.
instruction(west(N))      --> "W", integer(N), eol.
instruction(forward(N))   --> "F", integer(N), eol.

instruction(turn(left))   --> "L90",  eol.
instruction(turn(around)) --> "L180", eol.
instruction(turn(right))  --> "L270", eol.

instruction(turn(right))  --> "R90",  eol.
instruction(turn(around)) --> "R180", eol.
instruction(turn(left))   --> "R270", eol.

north(N, ship(D, X, Y0), ship(D, X, Y)) :- plus(N, Y0, Y).
east( N, ship(D, X0, Y), ship(D, X, Y)) :- plus(N, X0, X).
south(N, ship(D, X, Y0), ship(D, X, Y)) :- plus(N, Y, Y0).
west( N, ship(D, X0, Y), ship(D, X, Y)) :- plus(N, X, X0).

turn(left, ship(north, X, Y), ship(west,  X, Y)).
turn(left, ship(west,  X, Y), ship(south, X, Y)).
turn(left, ship(south, X, Y), ship(east,  X, Y)).
turn(left, ship(east,  X, Y), ship(north, X, Y)).

turn(around, ship(north, X, Y), ship(south, X, Y)).
turn(around, ship(east,  X, Y), ship(west,  X, Y)).
turn(around, ship(south, X, Y), ship(north, X, Y)).
turn(around, ship(west,  X, Y), ship(east,  X, Y)).

turn(right, Ship0, Ship) :- turn(left, Ship, Ship0), !.

forward(N, ship(north, X, Y0), ship(north, X, Y)) :- !, plus(N, Y0, Y).
forward(N, ship(east,  X0, Y), ship(east,  X, Y)) :- !, plus(N, X0, X).
forward(N, ship(south, X, Y0), ship(south, X, Y)) :- !, plus(N, Y, Y0).
forward(N, ship(west,  X0, Y), ship(west,  X, Y)) :- !, plus(N, X, X0).

part_one(Input, Result) :-
    foldl(call, Input, ship(east, 0, 0), ship(_, X, Y)),
    Result is abs(X) + abs(Y).

north(N, waypoint(X, Y0), waypoint(X, Y), Ship, Ship) :- plus(N, Y0, Y).
east( N, waypoint(X0, Y), waypoint(X, Y), Ship, Ship) :- plus(N, X0, X).
south(N, waypoint(X, Y0), waypoint(X, Y), Ship, Ship) :- plus(N, Y, Y0).
west( N, waypoint(X0, Y), waypoint(X, Y), Ship, Ship) :- plus(N, X, X0).

turn(left,   waypoint(X,  Y0), waypoint(Y, X), Ship, Ship) :- Y is -Y0.
turn(around, waypoint(X0, Y0), waypoint(X, Y), Ship, Ship) :- X is -X0, Y is -Y0.
turn(right,  waypoint(X0, Y ), waypoint(Y, X), Ship, Ship) :- X is -X0.

forward(N, Waypoint, Waypoint, ship(X0, Y0), ship(X, Y)) :-
    Waypoint = waypoint(WX, WY),
    X is X0 + N * WX,
    Y is Y0 + N * WY.

interpret([], WP, WP, S, S).
interpret([Instr | Instrs], WP0, WP, S0, S) :-
    call(Instr, WP0, WP1, S0, S1),
    interpret(Instrs, WP1, WP, S1, S).

part_two(Input, Result) :-
    interpret(Input, waypoint(10, 1), _, ship(0, 0), ship(X, Y)),
    Result is abs(X) + abs(Y).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
