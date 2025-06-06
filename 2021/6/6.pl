#!/usr/bin/env swipl
% usage: swipl 6.pl [FILENAME]

:- module(aoc2021day6, []).
:- module(aoc2021day6).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(solution_sequences)).

load(Result) --> sequence(integer, ",", Fish0), eol, eos,
    {
        msort(Fish0, Fish1),
        clumped(Fish1, Clumps),
        foldl([Fish - Count, R0, R] >> (
            nth0(Fish, R0, _, R1),
            nth0(Fish, R, Count, R1)
        ), Clumps, [0, 0, 0, 0, 0, 0, 0, 0, 0], Result)
    }.

population_model(Before, After) :-
    Before = [F0, F1, F2, F3, F4, F5, F6, F7, F8],
    Next   = [F1, F2, F3, F4, F5, F6, F,  F8, F0],
    F #= F0 + F7,
    ( After = Next ; population_model(Next, After) ).

population_after_n_days(N, Fish, Population) :-
    call_nth(population_model(Fish, FishN), N),
    sum_list(FishN, Population).

part_one(Fish, Population) :- population_after_n_days( 80, Fish, Population).
part_two(Fish, Population) :- population_after_n_days(256, Fish, Population).

main([File]) :-
    once(phrase_from_file(load(Fish), File)),
    part_one(Fish, ResultOne),
    writeln(ResultOne),
    part_two(Fish, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
