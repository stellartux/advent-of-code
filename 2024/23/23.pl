#!/usr/bin/env swipl
% usage: swipl 23.pl [FILENAME]

:- module(aoc2024day23, []).
:- module(aoc2024day23).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Graph) --> sequence(edge, Edges), eos, { vertices_edges_to_ugraph([], Edges, Graph) }.
edge(A - B) --> vertex(A0), "-", vertex(B0), eol, { sort([A0, B0], [A, B])}.
vertex(Vtx) --> [A, B], { atom_codes(Vtx, [A, B]) }.

triples(Graph, [V1, V2, V3]) :-
    vertices(Graph, Vertices),
    member(V1, Vertices),
    neighbours(V1, Graph, Neighbours1),
    member(V2, Neighbours1),
    neighbours(V2, Graph, Neighbours2),
    member(V3, Neighbours2),
    memberchk(V3, Neighbours1).

part_one(Graph, Result) :-
    aggregate_all(count, (
        triples(Graph, Triple),
        once((member(T, Triple), atom_chars(T, [t, _])))
    ), Result).

connected(Graph, [V | Vs]) :-
    vertices(Graph, Vertices),
    member(V, Vertices),
    neighbours(V, Graph, Neighbours),
    connected(Graph, Neighbours, Vs).

connected(Graph, Allowed, [V | Vs]) :-
    member(V, Allowed),
    neighbours(V, Graph, Neighbours),
    (   Neighbours == []
    ->  Vs = []
    ;   ord_intersection(Allowed, Neighbours, Allowed0),
        connected(Graph, Allowed0, Vs)
    ).

part_two(Graph, Result) :-
    aggregate_all(max(Length, Group), (
        connected(Graph, Group),
        length(Group, Length)
    ), max(_, Result)).

main([File]) :-
    once(phrase_from_file(load(Graph), File)),
    part_one(Graph, ResultOne),
    % 1,447,814 inferences, 0.125 CPU seconds
    writeln(ResultOne),
    part_two(Graph, ResultTwo),
    % 112,803,270 inferences, 8.652 CPU seconds
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
