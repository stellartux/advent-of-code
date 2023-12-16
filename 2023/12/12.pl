#!/usr/bin/env swipl

:- module(aoc2023day12, []).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- initialization(main, main).

lines([S|Ss], [G|Gs]) --> line(S, G), lines(Ss, Gs).
lines([], []) --> eos, !.

line(Springs, GroupSizes) -->
    string_without(" ", Springs), " ", sequence(integer, ",", GroupSizes), eol.

:- table count_options/3.

count_options([], [], 1) :- !.
count_options([46|Ss], Gs, Result) :- !,
    count_options(Ss, Gs, Result).
count_options([63|Ss], Gs, Result) :- !,
    once(count_options([35|Ss], Gs, Result1)),
    once(count_options([46|Ss], Gs, Result2)),
    Result #= Result1 + Result2.
count_options([35|Ss], Gs, Result) :- !,
    once(count_options(1, Ss, Gs, Result)).
count_options(_, _, 0).

count_options(N, [], [N], 1) :- !.
count_options(N, [46|Ss], [N|Gs], Result) :- !,
    count_options(Ss, Gs, Result).
count_options(N0, [35|Ss], Gs, Result) :- !,
    succ(N0, N1),
    count_options(N1, Ss, Gs, Result).
count_options(N, [63|Ss], Gs, Result) :- !,
    once(count_options(N, [35|Ss], Gs, Result1)),
    once(count_options(N, [46|Ss], Gs, Result2)),
    Result #= Result1 + Result2.
count_options(_, _, _, 0).

main(Springs, GroupSizes) :-
    maplist(count_options, Springs, GroupSizes, Results),
    sum_list(Results, Result),
    writeln(Result).

main([]) :-
    once(phrase_from_stream(lines(Springs, GroupSizes), user_input)),
    main(Springs, GroupSizes).

main([File]) :-
    once(phrase_from_file(lines(Springs, GroupSizes), File)),
    main(Springs, GroupSizes).
