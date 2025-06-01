#!/usr/bin/env swipl
% usage: swipl 24.pl [FILENAME]

:- module(aoc2021day24, []).
:- module(aoc2021day24).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

load(Result) --> read_dcg(Result), eol, eos.

read_dcg(Result) --> instruction(X), ( read_dcg(Y), { Result = (X, Y) } | { Result = X }).

register(w) --> "w".
register(x) --> "x".
register(y) --> "y".
register(z) --> "z".

instruction(inp(A))    --> "inp ", register(A), eol.
instruction(add(A, B)) --> "add ", register(A), " ", ( register(B) | integer(B) ), eol.
instruction(mul(A, B)) --> "mul ", register(A), " ", ( register(B) | integer(B) ), eol.
instruction(div(A, B)) --> "div ", register(A), " ", ( register(B) | integer(B) ), eol.
instruction(mod(A, B)) --> "mod ", register(A), " ", ( register(B) | integer(B) ), eol.
instruction(eql(A, B)) --> "eql ", register(A), " ", ( register(B) | integer(B) ), eol.

register(w, W, ALU, ALU) :- arg(1, ALU, W).
register(x, X, ALU, ALU) :- arg(2, ALU, X).
register(y, Y, ALU, ALU) :- arg(3, ALU, Y).
register(z, Z, ALU, ALU) :- arg(4, ALU, Z).

set_register(w, W, alu(_, X, Y, Z, I), alu(W, X, Y, Z, I)).
set_register(x, X, alu(W, _, Y, Z, I), alu(W, X, Y, Z, I)).
set_register(y, Y, alu(W, X, _, Z, I), alu(W, X, Y, Z, I)).
set_register(z, Z, alu(W, X, Y, _, I), alu(W, X, Y, Z, I)).
pop_input(I, alu(W, X, Y, Z, [I | Is]), alu(W, X, Y, Z, Is)).

inp(A)    --> pop_input(I), { I in 1..9 }, set_register(A, I).
add(A, B) --> register(A, X), ( register(B, Y) | { B = Y } ),
    { Z #= X + Y }, set_register(A, Z).
mul(A, B) --> register(A, X), ( register(B, Y) | { B = Y } ),
    { Z #= X * Y }, set_register(A, Z).
div(A, B) --> register(A, X), ( register(B, Y) | { B = Y } ),
    { Y #\= 0, Z #= X div Y }, set_register(A, Z).
mod(A, B) --> register(A, X), ( register(B, Y) | { B = Y } ),
    { X #>= 0, Y #> 0, Z #= X mod Y }, set_register(A, Z).
eql(A, B) --> register(A, X), ( register(B, Y) | { B = Y } ),
    ( { X = Y }, set_register(A, 1) | { X #\= Y }, set_register(A, 0) ).

find(Type, Input, Result) :-
    maplist({Type}/[X, Y] >> (Y =.. [Type, X]), Input, Options),
    once(labeling(Options, Input)),
    foldl([X, Y, Z] >> (Z is Y * 10 + X), Input, 0, Result).

main([File]) :-
    once(phrase_from_file(load(Instructions), File)),
    length(Input, 14),
    Input ins 1..9,
    once(call_dcg(Instructions, alu(0, 0, 0, 0, Input), alu(_, _, _, 0, []))),
    foreach(( find(max, Input, Result) ; find(min, Input, Result) ), writeln(Result)).

main([]) :- main(['input.txt']).
