#!/usr/bin/env swipl
% usage: swipl 1.pl FILENAME
% https://adventofcode.com/2023/day/1

:- module(aoc2023day1, []).
:- module(aoc2023day1).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(line, Result).
line(Line)   --> nonblanks(Line), { Line \= [] }, eol.

digit_integer(N) --> [C], { C in 0'0..0'9, number_codes(N, [C]) }.
lower_alpha(C)   --> [C], { C in 0'a..0'z }.

numbers1([])       --> eol.
numbers1(Ns)       --> lower_alpha(_), numbers1(Ns).
numbers1([N | Ns]) --> digit_integer(N), numbers1(Ns).

part_one(Input, Result) :-
    aggregate_all(sum(10 * N0 + N1), (
        member(Line, Input),
        once(phrase(numbers1(Ns), Line)),
        Ns = [N0 | _],
        last(Ns, N1)
    ), Result).

numbers2([])            --> eol.
numbers2(Ns) --> "o", numbers2o(Ns).
numbers2(Ns) --> "e", numbers2e(Ns).
numbers2(Ns) --> "n", numbers2n(Ns).
numbers2(Ns) --> "t", numbers2t(Ns).

numbers2([N | Ns]) --> digit_integer(N), numbers2(Ns).
numbers2([4 | Ns]) --> "four",           numbers2(Ns).
numbers2([5 | Ns]) --> "five",          numbers2e(Ns).
numbers2([6 | Ns]) --> "six",            numbers2(Ns).
numbers2([7 | Ns]) --> "seven",         numbers2n(Ns).
numbers2(Ns)       --> lower_alpha(_),   numbers2(Ns).

numbers2o([1 | Ns]) --> "ne",            numbers2e(Ns).
numbers2o(Ns)       --> numbers2(Ns).

numbers2t([2 | Ns]) --> "wo",            numbers2o(Ns).
numbers2t([3 | Ns]) --> "hree",          numbers2e(Ns).
numbers2t(Ns)       --> numbers2(Ns).

numbers2e([8 | Ns]) --> "ight",          numbers2t(Ns).
numbers2e(Ns)       --> numbers2(Ns).

numbers2n([9 | Ns]) --> "ine",          numbers2e(Ns).
numbers2n(Ns)       --> numbers2(Ns).

part_two(Input, Result) :-
    aggregate_all(sum(10 * N0 + N1), (
        member(Line, Input),
        once(phrase(numbers2(Ns), Line, _)),
        Ns = [N0 | _],
        last(Ns, N1)
    ), Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne).
