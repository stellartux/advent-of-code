#!/usr/bin/env swipl
% usage: swipl 24.pl [FILENAME]

:- module(aoc2015day24, []).
:- module(aoc2015day24).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(line, Result0), blanks, eos, { reverse(Result0, Result) }.
line(N) --> integer(N), eol.

quantum_entanglement([X | Xs], QE) :-
    foldl([A, B, C] >> (C is A * B), Xs, X, QE).

take_sum([X | Xs], Target, [X | Taken], Left) :-
    X < Target,
    plus(Target0, X, Target),
    take_sum(Xs, Target0, Taken, Left).

take_sum([X | Xs], X, [X], Xs).

take_sum([X | Xs], Target, Taken, [X | Left]) :-
    take_sum(Xs, Target, Taken, Left).

groups_of_three(Input, Groups) :-
    sum_list(Input, S0),
    0 is S0 mod 3,
    S is S0 / 3,
    findall(G, (
        take_sum(Input, S, G, I0),
        once(take_sum(I0, S, _, _))
    ), Groups).

groups_of_four(Input, Groups) :-
    sum_list(Input, S0),
    0 is S0 mod 4,
    S1 is S0 / 2,
    S2 is S0 / 4,
    findall(G, (
        take_sum(Input, S1, _, I0),
        once(take_sum(I0, S2, G, _))
    ), Groups).

minimum_quantum_entanglement(Groups, Result) :-
    aggregate_all(min(Len), (member(G, Groups), length(G, Len)), MinLength),
    include({MinLength}/[G] >> length(G, MinLength), Groups, Gs1),
    aggregate_all(min(QE), (member(G, Gs1), quantum_entanglement(G, QE)), Result).

part_one(Input, Result) :-
    groups_of_three(Input, Groups),
    minimum_quantum_entanglement(Groups, Result).

part_two(Input, Result) :-
    groups_of_four(Input, Groups),
    minimum_quantum_entanglement(Groups, Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
