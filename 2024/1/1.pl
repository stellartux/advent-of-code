#!/usr/bin/env swipl
% usage: swipl 1.pl [FILENAME=input.txt]

:- module(aoc2024day1, []).
:- module(aoc2024day1).
:- initialization(main, main).
:- use_module(library(dcg/basics)).

load_numbers([L | Ls], [R | Rs]) -->
    integer(L), blanks, integer(R), eol, !, load_numbers(Ls, Rs).
load_numbers([], []) --> eos.

part_one(Left, Right, Result) :-
    msort(Left, SortedLeft),
    msort(Right, SortedRight),
    foldl([L, R, S0, S] >> (S is S0 + abs(L - R)), SortedLeft, SortedRight, 0, Result).

count_map(Keys, CountMap) :-
    empty_assoc(CountMap0),
    foldl([Key, CM0, CM] >> (
        (   get_assoc(Key, CM0, Value0)
        ->  succ(Value0, Value)
        ;   Value = 1
        ),
        put_assoc(Key, CM0, Value, CM)
    ), Keys, CountMap0, CountMap).

part_two(Left, Right, Result) :-
    count_map(Right, Counts),
    foldl({Counts} / [Key, R0, R] >> (
        get_assoc(Key, Counts, Value)
    ->  R is R0 + Key * Value
    ;   R = R0
    ), Left, 0, Result).

main([File]) :-
    phrase_from_file(load_numbers(Left, Right), File),
    part_one(Left, Right, ResultOne),
    writeln(ResultOne),
    part_two(Left, Right, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(["input.txt"]).
