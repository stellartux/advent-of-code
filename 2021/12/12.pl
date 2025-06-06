#!/usr/bin/env swipl
% usage: swipl 12.pl [FILENAME]

:- module(aoc2021day12, []).
:- module(aoc2021day12).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(ugraphs)).

load(Graph) --> edges(Edges), eol, eos, { vertices_edges_to_ugraph([], Edges, Graph) }.

edges([Left-Right, Right-Left | Edges]) -->
    string_without("-", L), "-", nonblanks(R), eol,
    { atom_string(Left, L), atom_string(Right, R) },
    edges(Edges).
edges([]) --> eol.

big_cave(Name) :-
    atom_string(Name, String),
    string_upper(String, String).

walk(Retry, Graph, Path) :- walk(start, [], Retry, Graph, Path).
walk(Here, Seen0, Retry0, Graph, [Here | Path]) :-
    (   Here == end
    ->  Path = []
    ;   (   big_cave(Here)
        ->  Seen0 = Seen,
            Retry0 = Retry
        ;   (   ord_memberchk(Here, Seen0)
            ->  Here \== start, Retry0, Retry = false
            ;   Retry0 = Retry
            ),
            ord_add_element(Seen0, Here, Seen)
        ),
        neighbours(Here, Graph, Neighbours),
        member(Neighbour, Neighbours),
        walk(Neighbour, Seen, Retry, Graph, Path)
    ).

part_one(Graph, Result) :- aggregate_all(count, walk(false, Graph, _), Result).
part_two(Graph, Result) :- aggregate_all(count, walk(true,  Graph, _), Result).

main([File]) :-
    once(phrase_from_file(load(Graph), File)),
    part_one(Graph, ResultOne),
    writeln(ResultOne),
    part_two(Graph, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
