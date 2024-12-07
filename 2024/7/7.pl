#!/usr/bin/env swipl
% usage: swipl 7.pl [FILENAME]

:- module(aoc2024day7, []).
:- module(aoc2024day7).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(row, Result), { Result \= [] }, eos.

row(row(Target, Values)) --> integer(Target), ": ", sequence(integer, " ", Values), eol.

formula([X], _, X).
formula([X | Rest], Ops, Result) :-
    formula(Rest, Ops, Y),
    member(Op, Ops),
    Result =.. [Op, X, Y].

validate(Target, Ops, Values) :-
    formula(Values, Ops, Formula),
    evaluate(Formula, Target),
    !.

evaluate(X + Y, Target) :- !,
    evaluate(X, T0),
    evaluate(Y, T1),
    Target is T0 + T1.

evaluate(X * Y, Target) :- !,
    evaluate(X, T0),
    evaluate(Y, T1),
    Target is T0 * T1.

evaluate(cat(X, Y), Target) :- !,
    evaluate(X, T0),
    evaluate(Y, T1),
    cat(T1, T0, Target).

evaluate(X, X).

calibration(Input, Ops, Result) :-
    aggregate_all(sum(Target), (
        member(row(Target, Values), Input),
        reverse(Values, Values0),
        validate(Target, Ops, Values0)
    ), Result).

part_one(Input, Result) :-
    calibration(Input, [(+), (*)], Result).

cat(X, Y, Result) :-
    number_chars(X, Chars0),
    number_chars(Y, Chars1),
    append(Chars0, Chars1, Chars),
    number_chars(Result, Chars).

part_two(Input, Result) :-
    calibration(Input, [(+), (*), cat], Result).

main([File]) :-
    phrase_from_file(load(Input), File),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).

main([]) :- main(['input.txt']).
