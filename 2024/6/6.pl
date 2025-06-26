#!/usr/bin/env swipl
% usage: swipl 6.pl FILENAME
% https://adventofcode.com/2024/day/6

:- module(aoc2024day6, []).
:- module(aoc2024day6).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

load(map(Guard, Dir, Obs, Size)) --> map(0, 0, Guard, Dir, Obs, Size).
show(M) :- once(phrase(load(M), S)), format("~s\n", [S]).
show(M, M) :- show(M).

map(X, Y, _, _, [], yx(Y1, X)) --> { succ(Y, Y1) }, eol, eos.

map(X, Y, Guard, Dir, Obs, Size) --> { Size = yx(_, X) }, eol,
    { succ(Y, Y1) }, map(0, Y1, Guard, Dir, Obs, Size).

map(X, Y, Guard, Dir, Obs, Size) --> { Guard = yx(Y, X) }, direction(Dir),
    { succ(X, X1) }, map(X1, Y, Guard, Dir, Obs, Size).

map(X, Y, Guard, Dir, [yx(Y, X) | Obs], Size) --> "#",
    { succ(X, X1) }, map(X1, Y, Guard, Dir, Obs, Size).

map(X, Y, Guard, Dir, Obs, Size) --> ".",
    { succ(X, X1) }, map(X1, Y, Guard, Dir, Obs, Size).

direction(^) --> "^".
direction(v) --> "v".
direction(<) --> "<".
direction(>) --> ">".

ahead(^, yx(Y0, X), yx(Y, X)) :- succ(Y, Y0).
ahead(v, yx(Y0, X), yx(Y, X)) :- succ(Y0, Y).
ahead(<, yx(Y, X0), yx(Y, X)) :- succ(X, X0).
ahead(>, yx(Y, X0), yx(Y, X)) :- succ(X0, X).

turn_right(^, >).
turn_right(>, v).
turn_right(v, <).
turn_right(<, ^).
turn_right(map(G, D0, Os, S), map(G, D, Os, S)) :- turn_right(D0, D).

blocked_ahead(M, M) :- blocked_ahead(M).
blocked_ahead(map(Here, Dir, Obs, _)) :-
    ahead(Dir, Here, There),
    ord_memberchk(There, Obs).

move_forward(map(G0, D, Obs, S), Map) :-
    ahead(D, G0, G),
    Map = map(G, D, Obs, S),
    in_grid(Map).

in_grid(map(yx(YG, XG), _, _, yx(YS, XS))) :- XG #>= 0, XG #< XS, YG #>= 0, YG #< YS.

step --> ( blocked_ahead -> turn_right, step ; move_forward ).

%%  fixate(Goal) is multi.
fixate(Goal, S0, S) :- S0 = S ; call(Goal, S0, S1), fixate(Goal, S1, S).

part_one(Map0, Result) :-
    aggregate_all(count, Guard, fixate(step, Map0, map(Guard, _, _, _)), Result).

part_two(Input, Result) :-
    Input = Result.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne).
