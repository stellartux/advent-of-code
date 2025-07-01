#!/usr/bin/env swipl
% usage: swipl 22.pl FILENAME
% https://adventofcode.com/2019/day/22

:- module(aoc2019day22, []).
:- module(aoc2019day22).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Instructions) --> sequence(instruction, Instructions), eol, eos.

instruction(deal_into_new_stack)    --> "deal into new stack", eol.
instruction(cut_cards(N))           --> "cut ", integer(N), eol.
instruction(deal_with_increment(N)) --> "deal with increment ", integer(N), eol.

deal_into_new_stack(CardCount, Index0, Index) :-
    Index #= CardCount - Index0 - 1.

cut_cards(Cut, CardCount, Index0, Index) :-
    Index #= (Index0 - Cut) mod CardCount.

deal_with_increment(Increment, CardCount, Index0, Index) :-
    Index #= (Index0 * Increment) mod CardCount.

shuffle(CardCount, Instructions, Index0, Index) :-
    Index0 #>= 0, Index0 #< CardCount,
    foldl({CardCount}/[Instr, I0, I] >> call(Instr, CardCount, I0, I), Instructions, Index0, Index).

part_one(Instructions, Result) :- shuffle(10007, Instructions, 2019, Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne).
