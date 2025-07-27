#!/usr/bin/env swipl
% usage: swipl 10.pl FILENAME
% https://adventofcode.com/2020/day/10

:- module(aoc2020day10, []).
:- module(aoc2020day10).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Adaptors)   --> sequence(adaptor, Adaptors).
adaptor(Adaptor) --> integer(Adaptor), eol.

load(Filename, Diffs) :-
    once(phrase_from_file(load(Adaptors), Filename)),
    max_list(Adaptors, MaxJoltage),
    Device is MaxJoltage + 3,
    sort([Device | Adaptors], Joltages),
    diff([0 | Joltages], Diffs).

diff([_], []).
diff([A, B | ABs], [D | Ds]) :-
    plus(A, D, B),
    diff([B | ABs], Ds).

product([1 - Ones, 3 - Threes], Product) :- Product is Ones * Threes.

part_one --> msort, clumped, product.

distinct_arrangements(Clumped, Result) :- distinct_arrangements(Clumped, 1, Result).

distinct_arrangements([1 - Ones | Clumped], P0, P) :-
    nth1(Ones, [1, 2, 4, 7], P1),
    P2 is P0 * P1,
    distinct_arrangements(Clumped, P2, P).

distinct_arrangements([3 - _ | Clumped], P0, P) :-
    distinct_arrangements(Clumped, P0, P).

distinct_arrangements([], P, P).

part_two --> clumped, distinct_arrangements.

main([File]) :-
    load(File, Diffs),
    part_one(Diffs, ResultOne),
    writeln(ResultOne),
    part_two(Diffs, ResultTwo),
    writeln(ResultTwo).
