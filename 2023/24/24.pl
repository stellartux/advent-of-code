#!/usr/bin/env swipl

:- module(aoc2023day24, []).
:- module(aoc2023day24).
:- use_module(library(clpq)).
:- use_module(library(dcg/basics)).

hailstones([hs(Px, Py, Pz, Vx, Vy, Vz) | Hs]) -->
    integer(Px), ",", whites,
    integer(Py), ",", whites,
    integer(Pz), whites, "@", whites,
    integer(Vx), ",", whites,
    integer(Vy), ",", whites,
    integer(Vz), eol, !,
    hailstones(Hs).
hailstones([]) --> [].

pairwise([Left0|Rights], Left, Right) :-
    (Left0 = Left, member(Right, Rights)); pairwise(Rights, Left, Right).

collisions2d(Hailstones, Min, Max) :-
    pairwise(Hailstones, hs(Lpx, Lpy, _, Lvx, Lvy, _), hs(Rpx, Rpy, _, Rvx, Rvy, _)),
    {
        N >= 0,
        X = Lpx + N * Lvx,
        Y = Lpy + N * Lvy,
        M >= 0,
        X = Rpx + M * Rvx,
        Y = Rpy + M * Rvy,
        X >= Min,
        X =< Max,
        Y >= Min,
        Y =< Max
    }.

partone(Hailstones, Result) :-
    partone(200000000000000, 400000000000000, Hailstones, Result).
partone(Min, Max, Hs, Result) :-
    (   string(Hs)
    ->  phrase_from_file(hailstones(Hailstones), Hs)
    ;   Hs = Hailstones
    ),
    aggregate_all(count, collisions2d(Hailstones, Min, Max), Result).

