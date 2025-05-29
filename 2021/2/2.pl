#!/usr/bin/env swipl
% usage: swipl 2.pl [FILENAME]

:- module(aoc2021day2, []).
:- module(aoc2021day2).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Commands) --> sequence(command, Commands), eol, eos.

command(Command) --> instruction(Instr), whites, integer(Param), eol,
    { Command =.. [Instr, Param] }.

instruction(forward) --> "forward".
instruction(down) --> "down".
instruction(up) --> "up".

forward(N, submarine(X0, Y), submarine(X, Y)) :- !, plus(N, X0, X).
forward(N, submarine(X0, Y0, A), submarine(X, Y, A)) :-
    X is X0 + N,
    Y is Y0 + N * A.

down(N, submarine(X, Y0), submarine(X, Y)) :- !, plus(N, Y0, Y).
down(N, submarine(X, Y, A0), submarine(X, Y, A)) :- plus(N, A0, A).

up(N, submarine(X, Y0), submarine(X, Y)) :- !, plus(N, Y, Y0).
up(N, submarine(X, Y, A0), submarine(X, Y, A)) :- plus(N, A, A0).

interpret(Commands, State0, Result) :-
    foldl(call, Commands, State0, State),
    arg(1, State, X),
    arg(2, State, Y),
    Result is X * Y.

part_one(Commands, Result) :- interpret(Commands, submarine(0, 0), Result).
part_two(Commands, Result) :- interpret(Commands, submarine(0, 0, 0), Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
