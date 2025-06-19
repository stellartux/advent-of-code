#!/usr/bin/env swipl
% usage: swipl intcode.pl FILENAME

:- module(intcode, [interpret/2, interpret/3, interpret/4, load//1]).
:- module(intcode).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Program) --> sequence(integer, ",", Program), blanks.

%%  interpret(+VM0, -VM) is det.
%   Parse and executes the instructions, assuming no input.
interpret --> interpret([], _).
interpret(In, Out, VM) :- interpret(In, Out, VM, _).
interpret(In, Out) --> interpret(0, In, [], Out).

interpret(IP0, In0, Out0, Out) -->
    get_instruction(IP0, Instruction),
    (   { Instruction == 99 }
    ->  { Out0 = Out }
    ;   instruction(Instruction, In0, In1, Out0, Out1, IP0, IP1),
        interpret(IP1, In1, Out1, Out)
    ).

get_instruction(IP, Instruction, VM, VM) :-
    nth0(IP, VM, Opcode),
    Instruction #= Opcode mod 100.

get_argument(I0, I1, X, M, M) :-
    I #= I0 + I1,
    nth0(I0, M, Instr),
    Mode #= Instr div (10 ^ (I1 + 1)) mod 10,
    (   Mode == 0 % position
    ->  nth0(I, M, J), nth0(J, M, X)
    ;   Mode == 1, % immediate
        nth0(I, M, X)
    ).

set_argument(I0, I1, X, M0, M) :-
    I #= I0 + I1,
    % never in immediate mode
    nth0(I, M0, J),
    nth0(J, M0, _, M1),
    nth0(J, M, X, M1).

% add
instruction(1, In, In, Out, Out, IP0, IP) -->
    { IP #= IP0 + 4, C #= A + B },
    get_argument(IP0, 1, A),
    get_argument(IP0, 2, B),
    set_argument(IP0, 3, C).

% multiply
instruction(2, In, In, Out, Out, IP0, IP) -->
    { IP #= IP0 + 4, C #= A * B },
    get_argument(IP0, 1, A),
    get_argument(IP0, 2, B),
    set_argument(IP0, 3, C).

% input
instruction(3, [A | In], In, Out, Out, IP0, IP) -->
    { IP #= IP0 + 2 },
    set_argument(IP0, 1, A).

% output
instruction(4, In, In, Out, [A | Out], IP0, IP) -->
    { IP #= IP0 + 2 },
    get_argument(IP0, 1, A).

% jump-if-true
instruction(5, In, In, Out, Out, IP0, IP) -->
    (   get_argument(IP0, 1, 0)
    ->  { IP #= IP0 + 3 }
    ;   get_argument(IP0, 2, IP)
    ).

% jump-if-false
instruction(6, In, In, Out, Out, IP0, IP) -->
    (   get_argument(IP0, 1, 0)
    ->  get_argument(IP0, 2, IP)
    ;   { IP #= IP0 + 3 }
    ).

% less-than
instruction(7, In, In, Out, Out, IP0, IP) -->
    { IP #= IP0 + 4 },
    get_argument(IP0, 1, A),
    get_argument(IP0, 2, B),
    (   { A #< B }
    ->  set_argument(IP0, 3, 1)
    ;   set_argument(IP0, 3, 0)
    ).

% equals
instruction(8, In, In, Out, Out, IP0, IP) -->
    { IP #= IP0 + 4 },
    get_argument(IP0, 1, A),
    get_argument(IP0, 2, B),
    (   { A #= B }
    ->  set_argument(IP0, 3, 1)
    ;   set_argument(IP0, 3, 0)
    ).

:- begin_tests(intcode).

    test(interpret1) :-
        assertion(interpret([1,0,0,0,99], [2,0,0,0,99])).
    test(interpret2) :-
        assertion(interpret([2,3,0,3,99], [2,3,0,6,99])).
    test(interpret3) :-
        assertion(interpret([2,4,4,5,99,0], [2,4,4,5,99,9801])).
    test(interpret4) :-
        assertion(interpret([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99])).

:- end_tests(intcode).
