#!/usr/bin/env swipl

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

integer_nl(N) --> integer(N), "\n".

main([]) :- main(["2020/1/input.txt"]).
main([Filename]) :-
    phrase_from_file(sequence(integer_nl, Entries), Filename),
    member(N, Entries),
    member(M, Entries),
    M + N #= 2020, !,
    Result1 #= M * N,
    writeln(Result1),
    member(X, Entries),
    member(Y, Entries),
    member(Z, Entries),
    X + Y + Z #= 2020, !,
    Result2 #= X * Y * Z,
    writeln(Result2).
