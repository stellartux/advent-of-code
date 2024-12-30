#!/usr/bin/env swipl
% usage: swipl 19.pl [FILENAME]

:- module(aoc2018day19, []).
:- module(aoc2018day19).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load((IP, Instructions)) -->
    ip(IP), eol,
    sequence(instruction, Instructions), eos.

ip(IP) --> "#ip ", integer(IP).

instruction(Instruction) -->
    opcode(Opcode), " ", integer(A), " ", integer(B), " ", integer(C), eol,
    { Instruction =.. [Opcode, A, B, C] }.

opcode(addr) --> "addr".
opcode(addi) --> "addi".
opcode(mulr) --> "mulr".
opcode(muli) --> "muli".
opcode(banr) --> "banr".
opcode(bani) --> "bani".
opcode(borr) --> "borr".
opcode(bori) --> "bori".
opcode(setr) --> "setr".
opcode(seti) --> "seti".
opcode(gtir) --> "gtir".
opcode(gtri) --> "gtri".
opcode(gtrr) --> "gtrr".
opcode(eqir) --> "eqir".
opcode(eqri) --> "eqri".
opcode(eqrr) --> "eqrr".

addr(A, B, C, M0, M) :-
    nth0(A, M0, X),
    nth0(B, M0, Y),
    Z is X + Y,
    seti(Z, _, C, M0, M).

addi(A, B, C, M0, M) :-
    nth0(A, M0, X),
    Z is X + B,
    seti(Z, _, C, M0, M).

mulr(A, B, C, M0, M) :-
    nth0(A, M0, X),
    nth0(B, M0, Y),
    Z is X * Y,
    seti(Z, _, C, M0, M).

muli(A, B, C, M0, M) :-
    nth0(A, M0, X),
    Z is X * B,
    seti(Z, _, C, M0, M).

banr(A, B, C, M0, M) :-
    nth0(A, M0, X),
    nth0(B, M0, Y),
    Z is X /\ Y,
    seti(Z, _, C, M0, M).

bani(A, B, C, M0, M) :-
    nth0(A, M0, X),
    Z is X /\ B,
    seti(Z, _, C, M0, M).

borr(A, B, C, M0, M) :-
    nth0(A, M0, X),
    nth0(B, M0, Y),
    Z is X \/ Y,
    seti(Z, _, C, M0, M).

bori(A, B, C, M0, M) :-
    nth0(A, M0, X),
    Z is X \/ B,
    seti(Z, _, C, M0, M).

setr(A, _, C, M0, M) :-
    nth0(A, M0, X),
    seti(X, _, C, M0, M).

seti(A, _, C, M0, M) :-
    nth0(C, M0, _, M1),
    nth0(C, M, A, M1).

gtir(A, B, C, M0, M) :-
    nth0(B, M0, Y),
    ( A > Y -> X = 1 ; X = 0 ),
    seti(X, _, C, M0, M).

gtri(A, B, C, M0, M) :-
    nth0(A, M0, Y),
    ( Y > B -> X = 1 ; X = 0 ),
    seti(X, _, C, M0, M).

gtrr(A, B, C, M0, M) :-
    nth0(A, M0, Z),
    nth0(B, M0, Y),
    ( Z > Y -> X = 1 ; X = 0 ),
    seti(X, _, C, M0, M).

eqir(A, B, C, M0, M) :-
    nth0(B, M0, Y),
    ( A == Y -> X = 1 ; X = 0 ),
    seti(X, _, C, M0, M).

eqri(A, B, C, M0, M) :-
    nth0(A, M0, Y),
    ( Y == B -> X = 1 ; X = 0 ),
    seti(X, _, C, M0, M).

eqrr(A, B, C, M0, M) :-
    nth0(A, M0, Z),
    nth0(B, M0, Y),
    ( Z == Y -> X = 1 ; X = 0 ),
    seti(X, _, C, M0, M).

run(IP, Instructions, Machine0, Machine) :-
    (   nth0(IP, Machine0, N),
        nth0(N, Instructions, Instruction)
    ->  call(Instruction, Machine0, Machine1),
        addi(IP, 1, IP, Machine1, Machine2),
        run(IP, Instructions, Machine2, Machine)
    ;   addi(IP, -1, IP, Machine0, Machine)
    ).

part_one((IP, Instructions), Result) :-
    run(IP, Instructions, [0, 0, 0, 0, 0, 0], Machine),
    nth0(0, Machine, Result).

part_two((IP, Instructions), Result) :-
    run(IP, Instructions, [1, 0, 0, 0, 0, 0], Machine),
    nth0(0, Machine, Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    % 481,878,935 inferences, 29.590 CPU seconds (16285333 Lips)
    time(part_one(Input, ResultOne)),
    writeln(ResultOne),
    time(part_two(Input, ResultTwo)),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
