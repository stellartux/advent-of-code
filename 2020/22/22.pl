#!/usr/bin/env swipl
% usage: swipl 22.pl FILENAME

:- module(aoc2020day22, []).
:- module(aoc2020day22).
:- initialization(main, main).
:- use_module(library(dcg/basics)).

load((Player1, Player2)) -->
    "Player 1:", eol, deck(Player1),
    "Player 2:", eol, deck(Player2), eos.

deck([])       --> eol.
deck([I | Is]) --> integer(I), eol, deck(Is).

round1(P1s0, P2s0, Winner) :-
    (   P1s0 = [P1 | P1s1]
    ->  (   P2s0 = [P2 | P2s1]
        ->  (   P1 > P2
            ->  append(P1s1, [P1, P2], P1s2), P2s1 = P2s2
            ;   append(P2s1, [P2, P1], P2s2), P1s1 = P1s2
            ),
            round1(P1s2, P2s2, Winner)
        ;   Winner = P1s0
        )
    ;   Winner = P2s0
    ).

score(Cards, Score) :-
    length(Cards, Length),
    aggregate_all(sum(S), ( nth0(N, Cards, S0), S is (Length - N) * S0 ), Score).

part_one((Player1, Player2), Result) :-
    round1(Player1, Player2, WinningHand),
    score(WinningHand, Result).

round2(P1s0, P2s0, Seen0, Winner, WinningHand) :-
    (   ord_memberchk((P1s0, P2s0), Seen0)
    ->  WinningHand = P1s0, Winner = player1
    ;   ord_add_element(Seen0, (P1s0, P2s0), Seen),
        P1s0 = [P1 | P1s1]
    ->  (   P2s0 = [P2 | P2s1]
        ->  length(Prefix1, P1),
            length(Prefix2, P2),
            (   prefix(Prefix1, P1s1),
                prefix(Prefix2, P2s1)
            ->  round2(Prefix1, Prefix2, [], Subwinner, _),
                (   Subwinner == player1
                ->  append(P1s1, [P1, P2], P1s2), P2s2 = P2s1
                ;   P1s2 = P1s1, append(P2s1, [P2, P1], P2s2)
                )
            ;   P1 > P2
            ->  append(P1s1, [P1, P2], P1s2), P2s2 = P2s1
            ;   P1s2 = P1s1, append(P2s1, [P2, P1], P2s2)
            ),
            round2(P1s2, P2s2, Seen, Winner, WinningHand)
        ;   WinningHand = P1s0, Winner = player1
        )
    ;   WinningHand = P2s0, Winner = player2
    ).

part_two((Player1, Player2), Result) :-
    round2(Player1, Player2, [], _, WinningHand),
    score(WinningHand, Result).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
