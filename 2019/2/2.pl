#!/usr/bin/env swipl

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).
main([]) :- main(["2019/2/input.txt"]).
main([File]) :-
    phrase_from_file(program(Program0), File),
    Program0 = [Instr, _, _| Rest],
    append([Instr, 12, 2], Rest, Program1),
    interpret(Program1, [A|_]), !,
    writeln(A),
    append([Instr, X, Y], Rest, Program2),
    interpret(Program2, [19690720|_]), !,
    format("~|~`0t~d~2+~|~`0t~d~2+", [X, Y]).

program(Program) --> sequence(integer, ",", Program), blanks.

select_nth0(N, XList, Y, YList) :-
    nth0(N, XList, _, Rest),
    nth0(N, YList, Y, Rest).

interpret(VM0, VM) :-
    interpret(0, VM0, VM).

interpret(IP0, VM0, VM) :-
    nth0(IP0, VM0, Opcode),
    Instruction #= Opcode mod 100,
    (   Instruction #= 99
    ->  VM0 = VM
    ;   instruction(Instruction, IP0, IP1, VM0, VM1),
        interpret(IP1, VM1, VM)
    ).

arguments(IP, VM, A, B, C) :-
    IP1 #= IP + 1,
    IP2 #= IP + 2,
    IP3 #= IP + 3,
    nth0(IP1, VM, AI),
    nth0(AI, VM, A),
    nth0(IP2, VM, BI),
    nth0(BI, VM, B),
    nth0(IP3, VM, C).

instruction(1, IP0, IP, VM0, VM) :-
    arguments(IP0, VM0, A, B, C),
    ElemC #= A + B,
    select_nth0(C, VM0, ElemC, VM),
    IP #= IP0 + 4.

instruction(2, IP0, IP, VM0, VM) :-
    arguments(IP0, VM0, A, B, C),
    ElemC #= A * B,
    select_nth0(C, VM0, ElemC, VM),
    IP #= IP0 + 4.

:- begin_tests(y2019day2).

    test(interpret1) :-
        assertion(interpret([1,0,0,0,99], [2,0,0,0,99])).
    test(interpret2) :-
        assertion(interpret([2,3,0,3,99], [2,3,0,6,99])).
    test(interpret3) :-
        assertion(interpret([2,4,4,5,99,0], [2,4,4,5,99,9801])).
    test(interpret4) :-
        assertion(interpret([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99])).

:- end_tests(y2019day2).
