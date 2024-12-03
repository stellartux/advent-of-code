#!/usr/bin/env swipl
% usage: swipl 3.pl [FILENAME]

:- module(aoc2024day3, []).
:- module(aoc2024day3).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

integer999(X) --> integer(X), { X in 0..999 }.

load_token(Token) --> string(_), token(Token).

token(mul(A, B)) --> "mul(", integer999(A), ",", integer999(B), ")".
token(do)        --> "do()".
token(dont)      --> "don't()".

load(Input) --> sequence(load_token, Input), remainder(_).

part_one --> include([X] >> functor(X, mul, 2)), part_two.

part_two(Input, Result) :- phrase(interpret(1, 0, Result), Input).

interpret(P, R0, R) --> [mul(A, B)], { R1 #= R0 + A * B * P }, interpret(P, R1, R).
interpret(_, R0, R) --> [do],                                  interpret(1, R0, R).
interpret(_, R0, R) --> [dont],                                interpret(0, R0, R).
interpret(_, R,  R) --> eos.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
