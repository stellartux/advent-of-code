#!/usr/bin/env swipl
% usage: swipl 6.pl FILENAME
% https://adventofcode.com/2020/day/6

:- module(aoc2020day6, []).
:- module(aoc2020day6).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Groups) :- once(phrase_from_file(load(Groups), Filename)).

load(Groups) --> sequence(string, "\n\n", Groups).

part_one(Groups, Result) :-
    aggregate_all(sum(Count), (
        member(Group, Groups),
        aggregate_all(count, Answer, (
            member(Answer, Group),
            Answer \== 0'\n
        ), Count)
    ), Result).

part_two(Groups, Result) :-
    aggregate_all(sum(Count), (
        member(Group, Groups),
        once(phrase(sequence(nonblanks, blank, People), Group)),
        maplist(sort, People, Sorted),
        exclude(=([]), Sorted, PowerSet),
        ord_intersection(PowerSet, Answers),
        length(Answers, Count)
    ), Result).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
