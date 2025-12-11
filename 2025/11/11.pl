#!/usr/bin/env swipl
% usage: swipl 11.pl FILENAME
% https://adventofcode.com/2025/day/11

:- module(aoc2025day11, []).
:- module(aoc2025day11).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).

load(Result) --> sequence(line, Lines), { foldl(build_graph, Lines, [], Result) }.

build_graph(device(ID, Outputs), Graph0, Graph) :-
    pairs_keys_values(Edges, IDs, Outputs),
    maplist(=(ID), IDs),
    add_edges(Graph0, Edges, Graph).

line(device(ID, Outputs)) --> device(ID), ": ", sequence(device, " ", Outputs), eol.

device(ID) --> string_without(": \n", Codes), { atom_codes(ID, Codes) }.

paths(Graph, [Here | Path0], Path) :-
    Path1 = [There, Here | Path0],
    neighbors(Here, Graph, Theres),
    member(There, Theres),
    (   There == out
    ->  Path = Path1
    ;   paths(Graph, Path1, Path)
    ).

part_one_load --> load, part_one.
part_one(Graph, Result) :- aggregate_all(count, paths(Graph, [you], _), Result).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne).
