#!/usr/bin/env swipl
% usage: swipl 23.pl [FILENAME]

:- module(aoc2015day23, []).
:- module(aoc2015day23).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Instructions) --> sequence(instruction, Instructions), blanks, eos.

instruction(hlf(R))    --> "hlf ", register(R), eol.
instruction(tpl(R))    --> "tpl ", register(R), eol.
instruction(inc(R))    --> "inc ", register(R), eol.
instruction(jmp(O))    --> "jmp ", integer(O),  eol.
instruction(jie(R, O)) --> "jie ", register(R), ", ", integer(O), eol.
instruction(jio(R, O)) --> "jio ", register(R), ", ", integer(O), eol.

register(a) --> "a".
register(b) --> "b".

get_register(a, A, r(A, _)).
get_register(b, B, r(_, B)).

set_register(a, A, r(_, B), r(A, B)).
set_register(b, B, r(A, _), r(A, B)).

hlf(N, 1, R0, R) :-
    get_register(N, X0, R0),
    X #= X0 // 2,
    set_register(N, X, R0, R).

tpl(N, 1, R0, R) :-
    get_register(N, X0, R0),
    X #= X0 * 3,
    set_register(N, X, R0, R).

inc(N, 1, R0, R) :-
    get_register(N, X0, R0),
    succ(X0, X),
    set_register(N, X, R0, R).

jmp(O, O, R, R).

jie(N, O0, O, R, R) :-
    get_register(N, X, R),
    (   X mod 2 =:= 0
    ->  O = O0
    ;   O = 1
    ).

jio(N, O0, O, R, R) :-
    (   get_register(N, 1, R)
    ->  O = O0
    ;   O = 1
    ).

interpret(Instrs, I0, R0, R) :-
    (   nth0(I0, Instrs, Instr)
    ->  call(Instr, I1, R0, R1),
        plus(I0, I1, I2),
        interpret(Instrs, I2, R1, R)
    ;   R = R0
    ).

part_one(Instrs, Result) :- interpret(Instrs, 0, r(0, 0), r(_, Result)).
part_two(Instrs, Result) :- interpret(Instrs, 0, r(1, 0), r(_, Result)).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
