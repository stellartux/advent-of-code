#!/usr/bin/env swipl
% usage: swipl 17.pl [FILENAME]

:- module(aoc2024day17, []).
:- module(aoc2024day17).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load((machine(A, B, C, []), Program)) -->
    "Register A: ", integer(A), eol,
    "Register B: ", integer(B), eol,
    "Register C: ", integer(C), eol,
    eol,
    "Program: ", sequence(instruction, ",", Program), eol, eos.

instruction(instr(Opcode, Operand)) --> opcode(Opcode), ",", integer(Operand).

opcode(adv) --> "0".
opcode(bxl) --> "1".
opcode(bst) --> "2".
opcode(jnz) --> "3".
opcode(bxc) --> "4".
opcode(out) --> "5".
opcode(bdv) --> "6".
opcode(cdv) --> "7".

combo(Combo, A, B, C, R) :-
    (   Combo = 4
    ->  R = A
    ;   Combo = 5
    ->  R = B
    ;   Combo = 6
    ->  R = C
    ;   R = Combo
    ).

interpret(adv, Combo, Step0, Step, machine(A0, B, C, O), machine(A, B, C, O)) :-
    succ(Step0, Step),
    combo(Combo, A0, B, C, Value),
    A is A0 div (2 ^ Value).

interpret(bxl, Value, Step0, Step, machine(A, B0, C, O), machine(A, B, C, O)) :-
    succ(Step0, Step),
    B is B0 xor Value.

interpret(bst, Combo, Step0, Step, machine(A, B0, C, O), machine(A, B, C, O)) :-
    succ(Step0, Step),
    combo(Combo, A, B0, C, Value),
    B is Value mod 8.

interpret(jnz, Literal, Step0, Step, M, M) :-
    (   M = machine(0, _, _, _)
    ->  succ(Step0, Step)
    ;   Step is Literal div 2
    ).

interpret(bxc, _, Step0, Step, machine(A, B0, C, O), machine(A, B, C, O)) :-
    succ(Step0, Step),
    B is B0 xor C.

interpret(out, Combo, Step0, Step, machine(A, B, C, O0), machine(A, B, C, O)) :-
    succ(Step0, Step),
    combo(Combo, A, B, C, Value),
    Out is Value mod 8,
    once(append(O0, [Out], O)).

interpret(bdv, Combo, Step0, Step, machine(A, B0, C, O), machine(A, B, C, O)) :-
    succ(Step0, Step),
    combo(Combo, A, B0, C, Value),
    B is A div (2 ^ Value).

interpret(cdv, Combo, Step0, Step, machine(A, B, C0, O), machine(A, B, C, O)) :-
    succ(Step0, Step),
    combo(Combo, A, B, C0, Value),
    C is A div (2 ^ Value).

run(Step0, Program, Machine0, Machine) :-
    (   nth0(Step0, Program, instr(Opcode, Operand))
    ->  interpret(Opcode, Operand, Step0, Step, Machine0, Machine1),
        run(Step, Program, Machine1, Machine)
    ;   Machine0 = Machine
    ).

part_one((Machine0, Program), Result) :-
    run(0, Program, Machine0, machine(_, _, _, Result)).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne).

main([]) :- main(['input.txt']).
