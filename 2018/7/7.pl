#!/usr/bin/env swipl
% usage: swipl 7.pl [FILENAME]

:- module(aoc2018day7, []).
:- module(aoc2018day7).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ugraphs)).

load(Lines) --> sequence(line, Lines), eol, eos.

line(A - B) --> "Step ", [C], " must be finished before step ", [D], " can begin.", eol,
    { char_code(A, C), char_code(B, D) }.

part_two(Input, []).

main([File]) :-
    once(phrase_from_file(load(Lines), File)),
    vertices_edges_to_ugraph([], Lines, Graph),
    top_sort(Graph, ResultOne),
    format("~s\n", [ResultOne]),
    part_two(Graph, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
