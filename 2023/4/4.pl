#!/usr/bin/env swipl
% usage: swipl 4.pl FILENAME
% https://adventofcode.com/2023/day/4

:- module(aoc2023day4, []).
:- module(aoc2023day4).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(scratchcard, Result).

scratchcard(card(N0, Ns1, Ns2)) -->
    "Card", whites, integer(N0), ":", whites, card_numbers(Ns1), card_numbers(Ns2).

card_numbers([N | Ns]) --> integer(N), whites, card_numbers(Ns).
card_numbers([])       --> "|", whites ; eol.

score(card(_, Ns1, Ns2), Score) :-
    aggregate_all(count, ( member(N1, Ns2), memberchk(N1, Ns1) ), Score).

part_one(Scratchcards, Result) :-
    aggregate_all(sum(Score), (
        member(Card, Scratchcards),
        score(Card, Score0),
        ( Score0 == 0 -> Score = 0 ; Score #= 2 ^ (Score0 - 1) )
    ), Result).

play([], Counts, Counts).
play([Card0 | Cards0], Counts0, Counts) :-
    arg(1, Card0, CardNumber),
    rb_lookup(CardNumber, Count, Counts0),
    score(Card0, Score),
    (   numlist(1, Score, NextCards)
    ->  foldl(play_(Cards0, Count), NextCards, Counts0, Counts1)
    ;   Counts0 = Counts1
    ),
    play(Cards0, Counts1, Counts).

play_(Cards0, Count0, Index, Counts0, Counts) :-
    Count #= Count0 + Count1,
    nth1(Index, Cards0, Card),
    arg(1, Card, CardNumber),
    rb_update(Counts0, CardNumber, Count1, Count, Counts).

part_two(Scratchcards, Result) :-
    length(Scratchcards, Length),
    numlist(1, Length, Counts0),
    maplist([C, C - 1] >> true, Counts0, Counts1),
    list_to_rbtree(Counts1, Counts2),
    play(Scratchcards, Counts2, Counts),
    rb_fold([_ - C, R0, R] >> plus(C, R0, R), Counts, 0, Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
