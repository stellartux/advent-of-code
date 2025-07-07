#!/usr/bin/env swipl
% usage: swipl 1.pl FILENAME
% https://adventofcode.com/2022/day/1

:- module(aoc2022day1, []).
:- module(aoc2022day1).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result)    --> sequence(food, Result), eos.
food(Ns)        --> { dif(Ns, []) }, food_(Ns).
food_([N | Ns]) --> integer(N), eol, food_(Ns).
food_([])       --> eol.

both_parts(Foods, ResultOne, ResultTwo) :-
    maplist(sum_list, Foods, Sums),
    sort(0, @>=, Sums, [ResultOne, B, C | _]),
    ResultTwo is ResultOne + B + C.

main([File]) :-
    once(phrase_from_file(load(Foods), File)),
    both_parts(Foods, ResultOne, ResultTwo),
    writeln(ResultOne),
    writeln(ResultTwo).
