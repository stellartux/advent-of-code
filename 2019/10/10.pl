#!/usr/bin/env swipl
% usage: swipl 10.pl FILENAME

:- module(aoc2019day10, []).
:- module(aoc2019day10).
:- initialization(main, main).
:- use_module(library(dcg/basics)).

load(Asteroids) --> load(0, Asteroids0), { sort(Asteroids0, Asteroids) }.

load(_, [])        --> eol.
load(Y, Asteroids) --> load(0, Y, Asteroids).

load(_, Y, Asteroids)              --> eol,         { succ(Y, Y1) }, load(   Y1, Asteroids).
load(X, Y, [xy(X, Y) | Asteroids]) --> "#",         { succ(X, X1) }, load(X1, Y, Asteroids).
load(X, Y, Asteroids)              --> ("." | "X"), { succ(X, X1) }, load(X1, Y, Asteroids).

diff_step(xy(X0, Y0), xy(X1, Y1), xy(X, Y)) :-
    plus(X0, X2, X1),
    plus(Y0, Y2, Y1),
    (   ( X2 == 0 ; Y2 == 0)
    ->  X2 \== Y2,
        X is sign(X2),
        Y is sign(Y2)
    ;   F is gcd(X2, Y2),
        X is X2 div F,
        Y is Y2 div F
    ).

step(xy(X0, Y0), xy(X1, Y1), xy(X, Y)) :-
    plus(X0, X1, X),
    plus(Y0, Y1, Y).

walk(Asteroids, Here, Target) :-
    diff_step(Here, Target, Step),
    walk(Asteroids, Step, Here, Target).

walk(Asteroids, Step, Here, Target) :-
    step(Here, Step, There),
    (   ord_memberchk(There, Asteroids)
    ->  Target = There
    ;   walk(Asteroids, Step, There, Target)
    ).

detect_asteroids(Asteroids, Detector, Asteroid) :-
    member(Asteroid, Asteroids),
    walk(Asteroids, Detector, Asteroid).

part_one(Asteroids, OptimalLocation, DetectableAsteroids) :-
    aggregate_all(max(Count, Here), (
        member(Here, Asteroids),
        aggregate_all(count, detect_asteroids(Asteroids, Here, _), Count)
    ), max(DetectableAsteroids, OptimalLocation)).

sort_by(By, List, Sorted) :-
    map_list_to_pairs(By, List, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

angle_from(xy(X0, Y0), xy(X1, Y1), Angle) :-
    Angle0 is atan(X1 - X0, Y0 - Y1),
    ( Angle0 < 0 -> Angle is Angle0 + 2 * pi ; Angle = Angle0 ).

vaporize(N0, Asteroids0, Location, Result) :-
    findall(A, detect_asteroids(Asteroids0, Location, A), Visible0),
    length(Visible0, VisibleCount),
    (   N0 > VisibleCount
    ->  N is N - VisibleCount,
        sort(Visible0, Visible1),
        ord_subtract(Asteroids0, Visible1, Asteroids),
        vaporize(N, Asteroids, Location, Result)
    ;   sort_by(angle_from(Location), Visible0, Visible1),
        nth1(N0, Visible1, Result)
    ).

part_two(Input, Result) :- part_two(200, Input, Result).

part_two(N, Asteroids, Result) :-
    part_one(Asteroids, Location, _),
    part_two(N, Asteroids, Location, Result).

part_two(N, Asteroids0, Location, Result) :-
    ord_del_element(Asteroids0, Location, Asteroids),
    vaporize(N, Asteroids, Location, xy(X, Y)),
    Result is 100 * X + Y.

main([File]) :-
    once(phrase_from_file(load(Asteroids), File)),
    part_one(Asteroids, Location, ResultOne),
    writeln(ResultOne),
    part_two(200, Asteroids, Location, ResultTwo),
    writeln(ResultTwo).
