#!/usr/bin/env swipl
% usage: swipl 22.pl [FILENAME]

:- module(aoc2018day22, []).
:- module(aoc2018day22).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(scan(Depth, xy(X, Y), ErosionLevels)) --> blanks,
    "depth", whites, ":", whites, integer(Depth), blanks,
    "target", whites, ":", whites, integer(X), whites, ",", whites, integer(Y), blanks, eos,
    { nth0(0, ErosionLevels, FirstRow), nth0(0, FirstRow, 0) }.

geologic_index(Scan, Coord, GeologicIndex) :-
    (   arg(2, Scan, Coord)
    ->  GeologicIndex = 0
    ;   Coord = xy(X, Y),
        (   Y == 0
        ->  GeologicIndex is X * 16807
        ;   X == 0
        ->  GeologicIndex is Y * 48271
        ;   succ(X1, X),
            succ(Y1, Y),
            erosion_level(Scan, xy(X1, Y), E0),
            erosion_level(Scan, xy(X, Y1), E1),
            GeologicIndex is E0 * E1
        )
    ).

erosion_level(Scan, xy(X, Y), ErosionLevel) :-
    Scan = scan(Depth, _, ErosionLevels),
    nth0(Y, ErosionLevels, Row),
    nth0(X, Row, ErosionLevel),
    once((
        ground(ErosionLevel)
    ;   geologic_index(Scan, xy(X, Y), GeologicIndex),
        ErosionLevel is (GeologicIndex + Depth) mod 20183
    )).

risk_level(Scan, Coord, RiskLevel) :-
    erosion_level(Scan, Coord, ErosionLevel),
    RiskLevel is ErosionLevel mod 3.

coord_range(xy(Left, Top), xy(Right, Bottom), xy(X, Y)) :-
    between(Top, Bottom, Y), between(Left, Right, X).

part_one(Scan, TotalRisk) :-
    arg(2, Scan, Target),
    aggregate_all(sum(RiskLevel), (
        coord_range(xy(0, 0), Target, Coord),
        risk_level(Scan, Coord, RiskLevel)
    ), TotalRisk).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    time(part_one(Input, ResultOne)),
    writeln(ResultOne).

main([]) :- main(['input.txt']).
