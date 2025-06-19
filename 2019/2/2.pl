#!/usr/bin/env swipl
% usage: swipl 2.pl [FILENAME]

:- module(aoc2019day2, []).
:- module(aoc2019day2).
:- initialization(main, main).
:- use_module('../intcode.pl', [interpret/2, load/3]).

main([File]) :-
    phrase_from_file(load([Instr, _, _ | Rest]), File),
    interpret([Instr, 12, 2 | Rest], [A | _]),
    writeln(A),
    % 611,697,809 inferences, 40.970 CPU in 41.121 seconds (100% CPU, 14930450 Lips)
    time(interpret([Instr, X, Y | Rest], [19690720 | _])),
    format("~|~`0t~d~2+~|~`0t~d~2+", [X, Y]).

main([]) :- main(['2019/2/input.txt']).
