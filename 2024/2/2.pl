#!/usr/bin/env swipl

:- module(aoc2024day2, []).
:- module(aoc2024day2).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).

load(Rows) --> sequence(row, Rows).
row(Row) --> sequence(integer, " ", Row), { Row \= [] }, eol.

safe(Row) :-
    once((
        member(Range, [1..3, (-3)..(-1)]),
        forall(nextto(X, Y, Row), (Z #= X - Y, Z in Range))
    )).

part_one(Rows, Result) :-
    aggregate_all(count, (member(Row, Rows), safe(Row)), Result).

part_two(Rows, Result) :-
    aggregate_all(count, (
        member(Row, Rows), once(( select(_, Row, Row0), safe(Row0) ))
    ), Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
