#!/usr/bin/env swipl
% usage: swipl 2.pl FILENAME
% https://adventofcode.com/2025/day/2

:- module(aoc2025day2, []).
:- module(aoc2025day2).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).

load(Result) --> sequence(id_range, ",", Result), eol.

id_range(range(Low, High)) --> integer(Low), "-", integer(High).

invalid_id(ID) :-
    number_chars(ID, Chars),
    once(append(X, X, Chars)).

sum_invalid_ids(range(Low, High), Sum) :-
    aggregate_all(sum(ID), (
        between(Low, High, ID),
        invalid_id(ID)
    ), Sum).

part_one --> maplist(sum_invalid_ids), sum_list.

groups_of(_, [], []) :- !.
groups_of(N, List, Groups) :-
    length(Prefix, N),
    once(append(Prefix, Suffix, List)),
    Groups = [Prefix | Groups0],
    groups_of(N, Suffix, Groups0).

invalid_id_2(ID) :-
    number_chars(ID, Chars),
    length(Chars, Length),
    Max is Length - 1,
    between(1, Max, N),
    0 is Length mod N,
    groups_of(N, Chars, Groups),
    maplist(=(_), Groups),
    !.

sum_invalid_ids_2(range(Low, High), Sum) :-
    aggregate_all(sum(ID), (
        between(Low, High, ID),
        invalid_id_2(ID)
    ), Sum).

part_two --> maplist(sum_invalid_ids_2), sum_list.

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
