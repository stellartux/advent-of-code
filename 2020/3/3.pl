#!/usr/bin/env swipl
% usage: swipl 3.pl FILENAME
% https://adventofcode.com/2020/day/3

:- module(aoc2020day3, []).
:- module(aoc2020day3).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Grid) --> sequence(row, Grid), eol, eos.
row(Row)   --> nonblanks(Row), eol, { Row \= [] }.

go_skiing(Grid, slope(SlopeX, SlopeY), Result) :-
    Grid = [Row0 | _],
    length(Row0, Width),
    aggregate_all(count, (
        nth0(Y, Grid, Row),
        Y > 0,
        divmod(Y, SlopeY, Step, 0),
        X is (Step * SlopeX) mod Width,
        nth0(Y, Grid, Row),
        nth0(X, Row, 0'#)
    ), Result).

list_product([], P, P).
list_product([X | Xs], P0, P)   :- P1 is P0 * X, list_product(Xs, P1, P).
list_product([X | Xs], Product) :- list_product(Xs, X, Product).

main([File]) :-
    once(phrase_from_file(load(Grid), File)),
    maplist(go_skiing(Grid), [
        slope(1, 1),
        slope(3, 1),
        slope(5, 1),
        slope(7, 1),
        slope(1, 2)
    ], Results),
    nth1(2, Results, ResultOne),
    list_product(Results, ResultTwo),
    format("~d~n~d~n", [ResultOne, ResultTwo]).
