#!/usr/bin/env swipl
% usage: swipl intcode.pl FILENAME

/* The Intcode machine is used in 2019 days 2, 5, 7 and 9 */
:- module(intcode, [
    interpret/2,
    interpret/3,
    interpret/4,
    load//1,
    load_zero_extended//1,
    zero_extend/2
]).
:- module(intcode).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

zero_extend(Program0, Program) :-
    append([0], Zeroes, Zeroes),
    append(Program0, Zeroes, Program).

load(Program) --> sequence(integer, ",", Program), blanks.
load_zero_extended(Program) --> load(Program0), { zero_extend(Program0, Program) }.

%%  interpret(+VM0, -VM) is det.
%   Parse and executes the instructions, assuming no input.
interpret --> interpret([], _).
interpret(In, Out, VM) :- interpret(In, Out, VM, _).
interpret(In, Out) --> interpret(0, In, [], 0, Out).

interpret(IP0, In0, Out0, Base0, Out) -->
    get_instruction(IP0, Instruction),
    (   { Instruction == 99 }
    ->  { reverse(Out0, Out) }
    ;   instruction(Instruction, In0, In1, Out0, Out1, Base0, Base1, IP0, IP1),
        interpret(IP1, In1, Out1, Base1, Out)
    ).

get_mode(I0, I1, M, Mode) :-
    nth0(I0, M, Instr),
    Mode #= Instr div (10 ^ (I1 + 1)) mod 10.

get_instruction(IP, Instruction, VM, VM) :-
    nth0(IP, VM, Opcode),
    Instruction #= Opcode mod 100.

get_argument(I0, I1, Base, X, M, M) :-
    get_mode(I0, I1, M, Mode),
    I2 #= I0 + I1,
    nth0(I2, M, I3),
    (   Mode == 0
    ->  nth0(I3, M, X)
    ;   Mode == 1
    ->  I3 = X
    ;   Mode == 2
    ,   I4 #= I3 + Base,
        nth0(I4, M, X)
    ).

set_argument(I0, I1, Base, X, M0, M) :-
    get_mode(I0, I1, M0, Mode),
    I2 #= I0 + I1,
    % never in immediate mode
    nth0(I2, M0, I3),
    ( Mode == 2 -> I4 #= I3 + Base ; I4 = I3 ),
    nth0(I4, M0, _, M1),
    nth0(I4, M, X, M1).

% add
instruction(1, In, In, Out, Out, Base, Base, IP0, IP) -->
    { IP #= IP0 + 4, C #= A + B },
    get_argument(IP0, 1, Base, A),
    get_argument(IP0, 2, Base, B),
    set_argument(IP0, 3, Base, C).

% multiply
instruction(2, In, In, Out, Out, Base, Base, IP0, IP) -->
    { IP #= IP0 + 4, C #= A * B },
    get_argument(IP0, 1, Base, A),
    get_argument(IP0, 2, Base, B),
    set_argument(IP0, 3, Base, C).

% input
instruction(3, [A | In], In, Out, Out, Base, Base, IP0, IP) -->
    { IP #= IP0 + 2 },
    set_argument(IP0, 1, Base, A).

% output
instruction(4, In, In, Out, [A | Out], Base, Base, IP0, IP) -->
    { IP #= IP0 + 2 },
    get_argument(IP0, 1, Base, A).

% jump-if-true
instruction(5, In, In, Out, Out, Base, Base, IP0, IP) -->
    (   get_argument(IP0, 1, Base, 0)
    ->  { IP #= IP0 + 3 }
    ;   get_argument(IP0, 2, Base, IP)
    ).

% jump-if-false
instruction(6, In, In, Out, Out, Base, Base, IP0, IP) -->
    (   get_argument(IP0, 1, Base, 0)
    ->  get_argument(IP0, 2, Base, IP)
    ;   { IP #= IP0 + 3 }
    ).

% less-than
instruction(7, In, In, Out, Out, Base, Base, IP0, IP) -->
    { IP #= IP0 + 4 },
    get_argument(IP0, 1, Base, A),
    get_argument(IP0, 2, Base, B),
    (   { A #< B }
    ->  set_argument(IP0, 3, Base, 1)
    ;   set_argument(IP0, 3, Base, 0)
    ).

% equals
instruction(8, In, In, Out, Out, Base, Base, IP0, IP) -->
    { IP #= IP0 + 4 },
    get_argument(IP0, 1, Base, A),
    get_argument(IP0, 2, Base, B),
    (   { A #= B }
    ->  set_argument(IP0, 3, Base, 1)
    ;   set_argument(IP0, 3, Base, 0)
    ).

% change relative base
instruction(9, In, In, Out, Out, Base0, Base, IP0, IP) -->
    { IP #= IP0 + 2, Base #= Base0 + A },
    get_argument(IP0, 1, Base0, A).

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
