#!/usr/bin/env swipl
% usage: swipl 19.pl [ELF_COUNT=5]

:- module(aoc2016day19, []).
:- module(aoc2016day19).
:- initialization(main, main).

drop_half([X, _ | Tail0], [X | Tail], Parity) :- !, drop_half(Tail0, Tail, Parity).
drop_half([X], [X], odd).
drop_half([], [], even).

play([Result], Result) :- !.
play(Elves0, Result) :-
    drop_half(Elves0, Elves1, Parity),
    (   Parity == even
    ->  Elves1 = Elves
    ;   Elves1 = [_ | Elves]
    ),
    play(Elves, Result).

part_one(Input, Result) :-
    numlist(1, Input, Elves),
    play(Elves, Result).

main([N]) :-
    atom_number(N, Length),
    part_one(Length, ResultOne),
    writeln(ResultOne).

main([]) :- main(['5']).
