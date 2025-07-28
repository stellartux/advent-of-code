#!/usr/bin/env swipl
% usage: swipl 19.pl FILENAME
% https://adventofcode.com/2020/day/19

:- module(aoc2020day19, []).
:- module(aoc2020day19).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :-
    once(phrase_from_file(load(Input), Filename)).

load((Rules, Lines)) --> rules(Rules), sequence(line, Lines).

rules(Rules) -->
    (   eol, { once(append(Rules, [], Rules)) }
    |   rule(Rules), rules(Rules)
    ).

rule(Rules)  --> integer(N), ": ", { nth0(N, Rules, Rule) }, disj(Rules, Rule).

disj(Rules, Disj) --> conj(Rules, Conj),
    (   " | ", { Disj = (Conj ; Disj0) }, disj(Rules, Disj0)
    |   eol, { Disj = Conj }
    ).

conj(Rules, Conj) --> match(Rules, Match),
    (   " ", { Conj = (Match, Match0) }, conj(Rules, Match0)
    |   { Conj = Match }
    ).

match(Rules, Match) --> integer(N), { nth0(N, Rules, Match) }.
match(_,     Match) --> "\"", string_without("\"", Match0), "\"",
    { string_codes(Match, Match0) }.

line(Line)  --> nonblanks(Line), eol, { Line \= [] }.

part_one(([Rule | _], Lines), Result) :-
    aggregate_all(count, (
        member(Line, Lines),
        once(phrase(Rule, Line))
    ), Result).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne).
