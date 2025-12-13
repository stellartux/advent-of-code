#!/usr/bin/env swipl
% usage: swipl 11.pl FILENAME
% https://adventofcode.com/2025/day/11

:- module(aoc2025day11, []).
:- module(aoc2025day11).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).

load(Graph) --> sequence(line, Graph0), { keysort(Graph0, Graph) }.
line(ID - Outputs) --> device(ID), ": ", sequence(device, " ", Outputs0), eol, { sort(Outputs0, Outputs) }.

device(ID) --> string_without(": \n", Codes), { atom_codes(ID, Codes) }.

paths(Graph, Target, [Here | Path0], Path) :-
    Path1 = [There, Here | Path0],
    neighbors(Here, Graph, Theres),
    member(There, Theres),
    (   There == Target
    ->  Path = Path1
    ;   paths(Graph, Target, Path1, Path)
    ).

count_paths(Start, End, Graph0, Result) :-
    top_sort(Graph0, Sort),
    once(append(Vs0, [Start | Tail], Sort)),
    once(append(_, [End | Vs1], Tail)),
    del_vertices(Graph0, Vs0, Graph1),
    del_vertices(Graph1, Vs1, Graph),
    aggregate_all(count, paths(Graph, End, [Start], _), Result).

part_one_load --> load, part_one.
part_one --> count_paths(you, out).

part_two_load --> load, part_two.
part_two(Graph, Result) :-
    count_paths(svr, fft, Graph, Count1),
    count_paths(fft, dac, Graph, Count2),
    count_paths(dac, out, Graph, Count3),
    Result is Count1 * Count2 * Count3.

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    % 79,411,229,618 inferences, 10944.550 CPU in 10983.536 seconds (100% CPU, 7255779 Lips)
    time(part_two(Input, ResultTwo)),
    writeln(ResultTwo).
