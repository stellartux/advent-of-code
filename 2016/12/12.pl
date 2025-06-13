#!/usr/bin/env swipl
% usage: swipl 12.pl [FILENAME]

:- module(aoc2016day12, []).
:- module(aoc2016day12).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(instruction, Result), eol, eos.

instruction(cpy(X, Y)) --> "cpy ", ( register(X) | integer(X) ), " ", register(Y), eol.
instruction(inc(X))    --> "inc ", register(X), eol.
instruction(dec(X))    --> "dec ", register(X), eol.
instruction(jnz(X, Y)) --> "jnz ", ( register(X) | integer(X) ), " ", integer(Y), eol.

register(a) --> "a".
register(b) --> "b".
register(c) --> "c".
register(d) --> "d".

get_register(a, A, m(A, _, _, _)).
get_register(b, B, m(_, B, _, _)).
get_register(c, C, m(_, _, C, _)).
get_register(d, D, m(_, _, _, D)).

get_register(K, V, M, M) :- get_register(K, V, M).

set_register(a, A, m(_, B, C, D), m(A, B, C, D)).
set_register(b, B, m(A, _, C, D), m(A, B, C, D)).
set_register(c, C, m(A, B, _, D), m(A, B, C, D)).
set_register(d, D, m(A, B, C, _), m(A, B, C, D)).

cpy(X, Y, 1, M0, M) :-
    (   integer(X)
    ->  Val = X
    ;   get_register(X, Val, M0)
    ),
    set_register(Y, Val, M0, M).

inc(X, 1, M0, M) :-
    get_register(X, Val0, M0),
    Val is Val0 + 1,
    set_register(X, Val, M0, M).

dec(X, 1, M0, M) :-
    get_register(X, Val0, M0),
    Val is Val0 - 1,
    set_register(X, Val, M0, M).

jnz(X, Y, I, M, M) :-
    (   ( X == 0 ; get_register(X, 0, M) )
    ->  I = 1
    ;   I = Y
    ).

interpret(Instrs, I0, M0, M) :-
    (   nth0(I0, Instrs, Instr)
    ->  call(Instr, I1, M0, M1),
        I2 is I0 + I1,
        interpret(Instrs, I2, M1, M)
    ;   M0 = M
    ).

part_one(Input, Result) :-
    interpret(Input, 0, m(0, 0, 0, 0), m(Result, _, _, _)).

part_two(Input, Result) :-
    interpret(Input, 0, m(0, 0, 1, 0), m(Result, _, _, _)).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
