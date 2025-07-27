#!/usr/bin/env swipl
% usage: swipl 8.pl FILENAME
% https://adventofcode.com/2020/day/8

:- module(aoc2020day8, []).
:- module(aoc2020day8).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).
load(Instructions) --> sequence(instruction, Instructions), eol.

instruction(nop(X)) --> "nop ", integer(X), eol.
instruction(acc(X)) --> "acc ", integer(X), eol.
instruction(jmp(X)) --> "jmp ", integer(X), eol.

next_state(Instrs, S0, S) :-
    arg(1, S0, N0),
    nth0(N0, Instrs, Instr),
    call(Instr, S0, S).

acc(X, s(N0, A0), s(N, A)) :- plus(A0, X, A), succ(N0, N).
nop(_, s(N0,  A), s(N, A)) :- succ(N0, N).
jmp(X, s(N0,  A), s(N, A)) :- plus(N0, X, N).

execute(Instrs, Set0, S0, S) :-
    arg(1, S0, N0),
    fdset_add_element(Set0, N0, Set),
    (   next_state(Instrs, S0, S1)
    ->  arg(1, S1, N1),
        (   fdset_member(N1, Set)
        ->  S = S0
        ;   execute(Instrs, Set, S1, S)
        )
    ;   S0 = S
    ).

part_one(Instrs, Result) :-
    empty_fdset(Set),
    execute(Instrs, Set, s(0, 0), s(_, Result)).

swap_instruction(Instrs0, Instrs) :-
    (   nth0(N, Instrs0, nop(X), Instrs1), nth0(N, Instrs, jmp(X), Instrs1)
    ;   nth0(N, Instrs0, jmp(X), Instrs1), nth0(N, Instrs, nop(X), Instrs1)
    ).

part_two(Instrs, Result) :-
    length(Instrs, Length),
    once((
        swap_instruction(Instrs, Swapped),
        empty_fdset(Set),
        execute(Swapped, Set, s(0, 0), s(Length, Result))
    )).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
