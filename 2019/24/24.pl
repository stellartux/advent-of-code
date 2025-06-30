#!/usr/bin/env swipl
% usage: swipl 24.pl FILENAME
% https://adventofcode.com/2019/day/24

:- module(aoc2019day24, []).
:- module(aoc2019day24).
:- initialization(main, main).
:- use_module(library(dcg/basics)).

load(eris([A, B, C, D, E])) --> row(A), row(B), row(C), row(D), row(E), eol, eos.
row([A, B, C, D, E])        --> tile(A), tile(B), tile(C), tile(D), tile(E), eol.
tile(1) --> "#".
tile(0) --> ".".

coordinate(yx(Y, X), eris(Grid), Tile) :- nth0(Y, Grid, Row), nth0(X, Row, Tile).

coordinate(zyx(Z, Y, X), eris2(PositiveGrids, NegativeGrids), Tile) :-
    (   integer(Z)
    ->  (   Z >= 0
        ->  nth0(Z, PositiveGrids, Grid)
        ;   Z0 is -Z,
            nth1(Z0, NegativeGrids, Grid)
        )
    ;   (   nth0(Z, PositiveGrids, Grid)
        ;   nth1(Z0, NegativeGrids, Grid), Z is -Z0
        )
    ),
    nth0(Y, Grid, Row),
    (   Y == 2
    ->  nth0(X, Row, Tile), X \== 2
    ;   nth0(X, Row, Tile)
    ).

adjacent_tiles(yx(Y0, X0), Eris, Tile) :-
    (   Y = Y0, ( succ(X0, X) ; succ(X, X0) )
    ;   X = X0, ( succ(Y0, Y) ; succ(Y, Y0) )
    ),
    coordinate(yx(Y, X), Eris, Tile).

% outer tiles

adjacent_tiles(zyx(Z0, 0, _), Eris, Tile) :-
    Z is Z0 - 1,
    coordinate(zyx(Z, 1, 2), Eris, Tile).

adjacent_tiles(zyx(Z0, 4, _), Eris, Tile) :-
    Z is Z0 - 1,
    coordinate(zyx(Z, 3, 2), Eris, Tile).

adjacent_tiles(zyx(Z0, _, 0), Eris, Tile) :-
    Z is Z0 - 1,
    coordinate(zyx(Z, 2, 1), Eris, Tile).

adjacent_tiles(zyx(Z0, _, 4), Eris, Tile) :-
    Z is Z0 - 1,
    coordinate(zyx(Z, 2, 3), Eris, Tile).

% inner tiles

adjacent_tiles(zyx(Z0, 1, 2), Eris, Tile) :-
    Z is Z0 + 1,
    between(0, 4, X),
    coordinate(zyx(Z, 0, X), Eris, Tile).

adjacent_tiles(zyx(Z0, 3, 2), Eris, Tile) :-
    Z is Z0 + 1,
    between(0, 4, X),
    coordinate(zyx(Z, 4, X), Eris, Tile).

adjacent_tiles(zyx(Z0, 2, 1), Eris, Tile) :-
    Z is Z0 + 1,
    between(0, 4, Y),
    coordinate(zyx(Z, Y, 0), Eris, Tile).

adjacent_tiles(zyx(Z0, 2, 3), Eris, Tile) :-
    Z is Z0 + 1,
    between(0, 4, Y),
    coordinate(zyx(Z, Y, 4), Eris, Tile).

% neighbours on same level

adjacent_tiles(zyx(Z, Y0, X0), Eris, Tile) :-
    (   Y = Y0, ( succ(X0, X) ; succ(X, X0) )
    ;   X = X0, ( succ(Y0, Y) ; succ(Y, Y0) )
    ),
    once(( Y \== 2 ; X \== 2 )),
    coordinate(zyx(Z, Y, X), Eris, Tile).

next_tile(Eris, Coord, Tile0, Tile) :-
    aggregate_all(count, adjacent_tiles(Coord, Eris, 1), AdjacentBugs),
    (   Tile0 == 1
    ->  (   AdjacentBugs == 1
        ->  Tile = 1
        ;   Tile = 0
        )
    ;   (   between(1, 2, AdjacentBugs)
        ->  Tile = 1
        ;   Tile = 0
        )
    ).

:- meta_predicate maplist_with_nth0(3, +, -).
maplist_with_nth0(Goal, List0, List) :-
    length(List0, Length),
    succ(LastIndex, Length),
    numlist(0, LastIndex, N0s),
    maplist(Goal, N0s, List0, List).

:- meta_predicate maplist_with_nth1(3, +, -).
maplist_with_nth1(Goal, List0, List) :-
    length(List0, LastIndex),
    (   numlist(1, LastIndex, N0s)
    ->  maplist(Goal, N0s, List0, List)
    ;   List = []
    ).

:- meta_predicate map_eris(3, +, -).
map_eris(Goal, eris(Grid0), eris(Grid)) :- !,
    maplist_with_nth0({Goal}/[Y, Row0, Row] >> (
        maplist_with_nth0({Goal, Y}/[X, T0, T] >> (
            call(Goal, yx(Y, X), T0, T)
        ), Row0, Row)
    ), Grid0, Grid).

map_eris(Goal, eris2(PositiveGrids0, NegativeGrids0), eris2(PositiveGrids, NegativeGrids)) :- !,
    length(EmptyGrid, 5),
    maplist(=([0, 0, 0, 0, 0]), EmptyGrid),
    (   last(PositiveGrids0, EmptyGrid)
    ->  PositiveGrids0 = PositiveGrids1
    ;   append(PositiveGrids0, [EmptyGrid], PositiveGrids1)
    ),
    (   last(NegativeGrids0, EmptyGrid)
    ->  NegativeGrids0 = NegativeGrids1
    ;   append(NegativeGrids0, [EmptyGrid], NegativeGrids1)
    ),
    maplist_with_nth0({Goal}/[Z, Grid0, Grid] >> (
        maplist_with_nth0({Goal, Z}/[Y, Row0, Row] >> (
            maplist_with_nth0({Goal, Z, Y}/[X, T0, T] >> (
                call(Goal, zyx(Z, Y, X), T0, T)
            ), Row0, Row)
        ), Grid0, Grid)
    ), PositiveGrids1, PositiveGrids),
    maplist_with_nth1({Goal}/[Z0, Grid0, Grid] >> (
        Z is -Z0,
        maplist_with_nth0({Goal, Z}/[Y, Row0, Row] >> (
            maplist_with_nth0({Goal, Z, Y}/[X, T0, T] >> (
                call(Goal, zyx(Z, Y, X), T0, T)
            ), Row0, Row)
        ), Grid0, Grid)
    ), NegativeGrids1, NegativeGrids).

minute(Eris0, Eris) :- map_eris(next_tile(Eris0), Eris0, Eris).

biodiversity(Eris, Biodiversity) :-
    aggregate_all(sum(2 ^ X * 2 ^ (5 * Y)), coordinate(yx(Y, X), Eris, 1), Biodiversity).

:- meta_predicate find_repeat(2, +, -).
find_repeat(Goal, S0, S) :-
    empty_assoc(Seen0),
    put_assoc(S0, Seen0, 0, Seen),
    find_repeat(Goal, Seen, S0, S).

:- meta_predicate find_repeat(3, +, +, -).
find_repeat(Goal, Seen0, S0, S) :-
    call(Goal, S0, S1),
    (   get_assoc(S1, Seen0, 0)
    ->  S = S1
    ;   put_assoc(S1, Seen0, 0, Seen),
        find_repeat(Goal, Seen, S1, S)
    ).

eris_eris2(eris(Grid), eris2([Grid], [])).

fixate(N, Goal, State0, State) :-
    (   succ(N0, N)
    ->  call(Goal, State0, State1),
        fixate(N0, Goal, State1, State)
    ;   State0 = State
    ).

bug_count(Eris, BugCount) :- aggregate_all(count, coordinate(_, Eris, 1), BugCount).

part_one    --> find_repeat(minute), biodiversity.
part_two    --> part_two(200).
part_two(N) --> eris_eris2, fixate(N, minute), bug_count.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    % 48,995,013 inferences, 4.566 CPU in 4.581 seconds (100% CPU, 10730252 Lips)
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
