#!/usr/bin/env swipl
% usage: swipl 23.pl [FILENAME]

:- module(aoc2021day23, []).
:- module(aoc2021day23).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(map(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15)) -->
    "#############", eol,
    "#", space(S1), sequence(space, ".", [S2, S3, S4, S5, S6]), space(S7), "#", eol,
    "###", sequence(space, "#", [S8, S9, S10, S11]), "###", eol,
    "  #", sequence(space, "#", [S12, S13, S14, S15]), "#", eol,
    "  #########", eol, eos.

space(a) --> "A".
space(b) --> "B".
space(c) --> "C".
space(d) --> "D".
space(e) --> ".".

show(Map) :- once(phrase(load(Map), Codes)), format("~s\n", [Codes]).

energy_factor(a, 1).
energy_factor(b, 10).
energy_factor(c, 100).
energy_factor(d, 1000).

/*
####################################
#  1  2  _  3  _  4  _  5  _  6  7 #
#######  8 ##  9 ## 10 ## 11 #######
      # 12 ## 13 ## 14 ## 15 #
      ########################
*/

%% winning moves

% moving 1 to 12
step(E0 - map(a, e,   S3,     S4,    S5,    S6, S7,
                    e,    S9,    S10,   S11,
                    e,   S13,    S14,   S15),
      E - map(e, e,   S3,     S4,    S5,    S6, S7,
                    e,    S9,    S10,   S11,
                    a,   S13,    S14,   S15)
) :-
    E is E0 + 4, !.

% moving 2 to 12
step(E0 - map(S1, a,   S3,     S4,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     e,   S13,    S14,   S15),
      E - map(S1, e,   S3,     S4,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 3, !.

% moving 3 to 12
step(E0 - map(S1, S2,   a,     S4,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     e,   S13,    S14,   S15),
      E - map(S1, S2,   e,     S4,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 3, !.

% moving 4 to 12
step(E0 - map(S1, S2,   e,     a,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     e,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 5, !.

% moving 5 to 12
step(E0 - map(S1, S2,   e,     e,     a,    S6, S7,
                     e,    S9,    S10,   S11,
                     e,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,     e,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 7, !.

% moving 6 to 12
step(E0 - map(S1, S2,   e,     e,     e,     a, S7,
                     e,    S9,    S10,   S11,
                     e,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,     e,     e, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 9, !.

% moving 7 to 12
step(E0 - map(S1, S2,   e,     e,     e,     e, a,
                     e,    S9,    S10,   S11,
                     e,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,     e,     e, e,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 10, !.

% moving 1 to 13
step(E0 - map(b, e,    e,   S4,     S5,    S6, S7,
                    S8,   e,    S10,   S11,
                   S12,   e,    S14,   S15),
      E - map(e, e,    e,    S4,     S5,    S6, S7,
                    S8,   e,    S10,   S11,
                   S12,   b,    S14,   S15)
) :-
    E is E0 + 60, !.

% moving 2 to 13
step(E0 - map(S1, b,    e,   S4,     S5,    S6, S7,
                    S8,   e,    S10,   S11,
                   S12,   e,    S14,   S15),
      E - map(S1, e,    e,    S4,     S5,    S6, S7,
                    S8,   e,    S10,   S11,
                   S12,   b,    S14,   S15)
) :-
    E is E0 + 50, !.

% moving 3 to 13
step(E0 - map(S1, S2,   b,   S4,     S5,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   e,    S14,   S15),
      E - map(S1, S2,   e,    S4,     S5,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 30, !.

% moving 4 to 13
step(E0 - map(S1, S2,   S3,   b,     S5,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   e,    S14,   S15),
      E - map(S1, S2,   S3,   e,     S5,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 30, !.

% moving 5 to 13
step(E0 - map(S1, S2,   S3,   e,     b,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   e,    S14,   S15),
      E - map(S1, S2,   S3,   e,     e,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 50, !.

% moving 6 to 13
step(E0 - map(S1, S2,   S3,   e,     e,    b, S7,
                     S8,   e,    S10,   S11,
                    S12,   e,    S14,   S15),
      E - map(S1, S2,   S3,   e,     e,    e, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 70, !.

% moving 7 to 13
step(E0 - map(S1, S2,   S3,   e,     e,    e, b,
                     S8,   e,    S10,   S11,
                    S12,   e,    S14,   S15),
      E - map(S1, S2,   S3,   e,     e,    e, e,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 80, !.

% moving 1 to 14
step(E0 - map(c, e,    e,    e,   S5,    S6, S7,
                    S8,   S9,   e,   S11,
                   S12,  S13,   e,   S15),
      E - map(e, e,    e,    e,   S5,    S6, S7,
                    S8,   S9,   e,   S11,
                   S12,  S13,   c,   S15)
) :-
    E is E0 + 800, !.

% moving 2 to 14
step(E0 - map(S1, c,    e,    e,   S5,    S6, S7,
                    S8,   S9,    e,   S11,
                   S12,  S13,    e,   S15),
      E - map(S1, e,    e,    e,   S5,   S6, S7,
                    S8,   S9,    e,   S11,
                   S12,  S13,    c,   S15)
) :-
    E is E0 + 700, !.

% moving 3 to 14
step(E0 - map(S1, S2,   c,    e,   S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15),
      E - map(S1, S2,   e,    e,   S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 500, !.

% moving 4 to 14
step(E0 - map(S1, S2,   S3,   c,   S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15),
      E - map(S1, S2,   S3,   e,    S5,    S6, S7,
                     S8,   S9,    e,   S11,
                    S12,  S13,    c,   S15)
) :-
    E is E0 + 300, !.

% moving 5 to 14
step(E0 - map(S1, S2,   S3,   S4,   c,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15),
      E - map(S1, S2,   S3,   S4,   e,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 300, !.

% moving 6 to 14
step(E0 - map(S1, S2,   S3,   S4,   e,    c, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15),
      E - map(S1, S2,   S3,   S4,   e,    e, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 500, !.

% moving 7 to 14
step(E0 - map(S1, S2,   S3,   S4,   e,    e, c,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15),
      E - map(S1, S2,   S3,   S4,   e,    e, e,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 600, !.

% moving 1 to 15
step(E0 - map(d, e,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  e,
                   S12,  S13,  S14,  e),
      E - map(e, e,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  e,
                   S12,  S13,  S14,  d)
) :-
    E is E0 + 10000, !.

% moving 2 to 15
step(E0 - map(S1, d,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  e,
                   S12,  S13,  S14,  e),
      E - map(S1, e,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  e,
                   S12,  S13,  S14,  d)
) :-
    E is E0 + 9000, !.

% moving 3 to 15
step(E0 - map(S1, S2,    d,    e,    e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  e),
      E - map(S1, S2,    e,    e,    e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  d)
) :-
    E is E0 + 7000, !.

% moving 4 to 15
step(E0 - map(S1, S2,   S3,   d,    e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  e),
      E - map(S1, S2,   S3,    e,    e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  d)
) :-
    E is E0 + 5000, !.

% moving 5 to 15
step(E0 - map(S1, S2,   S3,   S4,   d,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  e),
      E - map(S1, S2,   S3,   S4,   e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  d)
) :-
    E is E0 + 3000, !.

% moving 6 to 15
step(E0 - map(S1, S2,   S3,   S4,   S5,  d, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e),
      E - map(S1, S2,   S3,   S4,   S5,  e, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   d)
) :-
    E is E0 + 3000, !.

% moving 7 to 15
step(E0 - map(S1, S2,   S3,   S4,   S5,  e, d,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e),
      E - map(S1, S2,   S3,   S4,   S5,  e, e,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   d)
) :-
    E is E0 + 3000, !.

% moving 1 to 8
step(E0 - map(a, e,   S3,     S4,    S5,    S6, S7,
                    e,    S9,    S10,   S11,
                    a,   S13,    S14,   S15),
      E - map(e, e,   S3,     S4,    S5,    S6, S7,
                    a,    S9,    S10,   S11,
                    a,   S13,    S14,   S15)
) :-
    E is E0 + 3, !.


% moving 2 to 8
step(E0 - map(S1, a,   S3,     S4,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15),
      E - map(S1, e,   S3,     S4,    S5,    S6, S7,
                     a,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 2, !.

% moving 3 to 8
step(E0 - map(S1, S2,   a,     S4,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15),
      E - map(S1, S2,   e,     S4,    S5,    S6, S7,
                     a,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 2, !.

% moving 4 to 8
step(E0 - map(S1, S2,   e,     a,    S5,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,    S5,    S6, S7,
                     a,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 4, !.

% moving 5 to 8
step(E0 - map(S1, S2,   e,     e,     a,    S6, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,     e,    S6, S7,
                     a,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 6, !.

% moving 6 to 8
step(E0 - map(S1, S2,   e,     e,     e,     a, S7,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,     e,     e, S7,
                     a,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 8, !.

% moving 7 to 8
step(E0 - map(S1, S2,   e,     e,     e,     e, a,
                     e,    S9,    S10,   S11,
                     a,   S13,    S14,   S15),
      E - map(S1, S2,   e,     e,     e,     e, e,
                     a,    S9,    S10,   S11,
                     a,   S13,    S14,   S15)
) :-
    E is E0 + 9, !.


% moving 1 to 9
step(E0 - map(b, e,    e,   S4,     S5,    S6, S7,
                    S8,   e,    S10,   S11,
                   S12,   b,    S14,   S15),
      E - map(e, e,    e,    S4,     S5,    S6, S7,
                    S8,   b,    S10,   S11,
                   S12,   b,    S14,   S15)
) :-
    E is E0 + 50, !.

% moving 2 to 9
step(E0 - map(S1, b,    e,   S4,     S5,    S6, S7,
                    S8,   e,    S10,   S11,
                   S12,   b,    S14,   S15),
      E - map(S1, e,    e,    S4,     S5,    S6, S7,
                    S8,   b,    S10,   S11,
                   S12,   b,    S14,   S15)
) :-
    E is E0 + 40, !.

% moving 3 to 9
step(E0 - map(S1, S2,   b,   S4,     S5,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15),
      E - map(S1, S2,   e,    S4,     S5,    S6, S7,
                     S8,   b,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 20, !.

% moving 4 to 9
step(E0 - map(S1, S2,   S3,   b,     S5,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15),
      E - map(S1, S2,   S3,   e,     S5,    S6, S7,
                     S8,   b,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 20, !.

% moving 5 to 9
step(E0 - map(S1, S2,   S3,   e,     b,    S6, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15),
      E - map(S1, S2,   S3,   e,     e,    S6, S7,
                     S8,   b,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 40, !.

% moving 6 to 9
step(E0 - map(S1, S2,   S3,   e,     e,    b, S7,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15),
      E - map(S1, S2,   S3,   e,     e,    e, S7,
                     S8,   b,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 60, !.

% moving 7 to 9
step(E0 - map(S1, S2,   S3,   e,     e,    e, b,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15),
      E - map(S1, S2,   S3,   e,     e,    e, e,
                     S8,   e,    S10,   S11,
                    S12,   b,    S14,   S15)
) :-
    E is E0 + 70, !.


% moving 1 to 10
step(E0 - map(c, e,    e,    e,   S5,    S6, S7,
                    S8,   S9,   e,   S11,
                   S12,  S13,   c,   S15),
      E - map(e, e,    e,    e,   S5,    S6, S7,
                    S8,   S9,   c,   S11,
                   S12,  S13,   c,   S15)
) :-
    E is E0 + 700, !.

% moving 2 to 10
step(E0 - map(S1, c,    e,    e,   S5,    S6, S7,
                    S8,   S9,    e,   S11,
                   S12,  S13,    c,   S15),
      E - map(S1, e,    e,    e,   S5,   S6, S7,
                    S8,   S9,    c,   S11,
                   S12,  S13,    c,   S15)
) :-
    E is E0 + 600, !.

% moving 3 to 10
step(E0 - map(S1, S2,   c,    e,     S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15),
      E - map(S1, S2,   e,    e,     S5,    S6, S7,
                     S8,   S9,   c,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 400, !.

% moving 4 to 10
step(E0 - map(S1, S2,   S3,   c,  S5,    S6, S7,
                     S8,   S9,  e,   S11,
                    S12,  S13,  c,   S15),
      E - map(S1, S2,   S3,   e,  S5,    S6, S7,
                     S8,   S9,  c,   S11,
                    S12,  S13,  c,   S15)
) :-
    E is E0 + 200, !.

% moving 5 to 10
step(E0 - map(S1, S2,   S3,   S4,   c,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15),
      E - map(S1, S2,   S3,   S4,   e,    S6, S7,
                     S8,   S9,   c,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 200, !.

% moving 6 to 10
step(E0 - map(S1, S2,   S3,   S4,   e,    c, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15),
      E - map(S1, S2,   S3,   S4,   e,    e, S7,
                     S8,   S9,   c,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 400, !.

% moving 7 to 10
step(E0 - map(S1, S2,   S3,   S4,   e,    e, c,
                     S8,   S9,   e,   S11,
                    S12,  S13,   c,   S15),
      E - map(S1, S2,   S3,   S4,   e,    e, e,
                     S8,   S9,   c,   S11,
                    S12,  S13,   c,   S15)
) :-
    E is E0 + 500, !.


% moving 1 to 11
step(E0 - map(d, e,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  e,
                   S12,  S13,  S14,  d),
      E - map(e, e,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  d,
                   S12,  S13,  S14,  d)
) :-
    E is E0 + 9000, !.

% moving 2 to 11
step(E0 - map(S1, d,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  e,
                   S12,  S13,  S14,  d),
      E - map(S1, e,    e,    e,    e,  S6, S7,
                    S8,   S9,  S10,  d,
                   S12,  S13,  S14,  d)
) :-
    E is E0 + 8000, !.

% moving 3 to 11
step(E0 - map(S1, S2,    d,    e,    e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  d),
      E - map(S1, S2,    e,    e,    e,  S6, S7,
                     S8,   S9,  S10,  d,
                    S12,  S13,  S14,  d)
) :-
    E is E0 + 6000, !.

% moving 4 to 11
step(E0 - map(S1, S2,   S3,   d,    e,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  d),
      E - map(S1, S2,   S3,    e,    e,  S6, S7,
                     S8,   S9,  S10,  d,
                    S12,  S13,  S14,  d)
) :-
    E is E0 + 4000, !.

% moving 5 to 11
step(E0 - map(S1, S2,   S3,   S4,   d,  S6, S7,
                     S8,   S9,  S10,  e,
                    S12,  S13,  S14,  d),
      E - map(S1, S2,   S3,   S4,   e,  S6, S7,
                     S8,   S9,  S10,  d,
                    S12,  S13,  S14,  d)
) :-
    E is E0 + 2000, !.

% moving 6 to 11
step(E0 - map(S1, S2,   S3,   S4,   S5,  d, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   d),
      E - map(S1, S2,   S3,   S4,   S5,  e, S7,
                     S8,   S9,  S10,   d,
                    S12,  S13,  S14,   d)
) :-
    E is E0 + 2000, !.

% moving 7 to 11
step(E0 - map(S1, S2,   S3,   S4,   S5,  e, d,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   d),
      E - map(S1, S2,   S3,   S4,   S5,  e, e,
                     S8,   S9,  S10,   d,
                    S12,  S13,  S14,   d)
) :-
    E is E0 + 3000, !.

% possible moves

% moving 12 to 1
step(E0 - map(e, e,     S3,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(A, e,     S3,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 4 * F + E0.

% moving 12 to 2
step(E0 - map(S1, e,     S3,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(S1, A,     S3,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 3 * F + E0.

% moving 12 to 3
step(E0 - map(S1, S2,    e,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(S1, S2,    A,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 3 * F + E0.

% moving 12 to 4
step(E0 - map(S1, S2,    e,     e,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(S1, S2,    e,     A,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 5 * F + E0.

% moving 12 to 5
step(E0 - map(S1, S2,    e,     e,    e,    S6, S7,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(S1, S2,    e,     e,     A,    S6, S7,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 7 * F + E0.

% moving 12 to 6
step(E0 - map(S1, S2,    e,     e,    e,    e, S7,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(S1, S2,    e,     e,    e,    A, S7,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 9 * F + E0.

% moving 12 to 7
step(E0 - map(S1, S2,    e,     e,    e,    e, e,
                      e,    S9,    S10,   S11,
                      A,   S13,    S14,   S15),
      E - map(S1, S2,    e,     e,    e,    e, A,
                      e,    S9,    S10,   S11,
                      e,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    dif(a, A),
    E is 10 * F + E0.

% moving 13 to 1
step(E0 - map(e, e,    e,    S4,    S5,    S6, S7,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(A, e,    e,    S4,    S5,    S6, S7,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 6 * F + E0.

% moving 13 to 2
step(E0 - map(S1, e,    e,    S4,    S5,    S6, S7,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(S1, A,    e,    S4,    S5,    S6, S7,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 5 * F + E0.

% moving 13 to 3
step(E0 - map(S1, S2,    e,    S4,    S5,    S6, S7,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(S1, S2,    A,    S4,    S5,    S6, S7,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 3 * F + E0.

% moving 13 to 4
step(E0 - map(S1, S2,   S3,    e,    S5,    S6, S7,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(S1, S2,   S3,    A,    S5,    S6, S7,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 3 * F + E0.

% moving 13 to 5
step(E0 - map(S1, S2,   S3,    e,    e,    S6, S7,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(S1, S2,   S3,    e,    A,    S6, S7,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 5 * F + E0.

% moving 13 to 6
step(E0 - map(S1, S2,   S3,    e,    e,    e, S7,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(S1, S2,   S3,    e,    e,    A, S7,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 7 * F + E0.

% moving 13 to 7
step(E0 - map(S1, S2,   S3,    e,    e,    e, e,
                    S8,    e,    S10,  S11,
                   S12,    A,    S14,  S15),
      E - map(S1, S2,   S3,    e,    e,    e, A,
                    S8,    e,   S10,   S11,
                   S12,    e,   S14,   S15)
) :-
    energy_factor(A, F),
    dif(b, A),
    E is 8 * F + E0.

% moving 14 to 1
step(E0 - map(e, e,    e,    e,    S5,    S6, S7,
                    S8,   S9,   e,   S11,
                   S12,  S13,   A,   S15),
      E - map(A, e,    e,    e,    S5,    S6, S7,
                    S8,   S9,   e,   S11,
                   S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 8 * F + E0.

% moving 14 to 2
step(E0 - map(S1, e,    e,    e,    S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   A,   S15),
      E - map(S1, A,    e,    e,    S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 7 * F + E0.

% moving 14 to 3
step(E0 - map(S1, S2,    e,    e,    S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   A,   S15),
      E - map(S1, S2,    A,    e,    S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 5 * F + E0.

% moving 14 to 4
step(E0 - map(S1, S2,   S3,    e,    S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   A,   S15),
      E - map(S1, S2,   S3,    A,    S5,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 3 * F + E0.

% moving 14 to 5
step(E0 - map(S1, S2,   S3,   S4,   e,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   A,   S15),
      E - map(S1, S2,   S3,   S4,   A,    S6, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 3 * F + E0.

% moving 14 to 6
step(E0 - map(S1, S2,   S3,   S4,   e,    e, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   A,   S15),
      E - map(S1, S2,   S3,   S4,   e,    A, S7,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 5 * F + E0.

% moving 14 to 7
step(E0 - map(S1, S2,   S3,   S4,   e,    e, e,
                     S8,   S9,   e,   S11,
                    S12,  S13,   A,   S15),
      E - map(S1, S2,   S3,   S4,   e,    e, A,
                     S8,   S9,   e,   S11,
                    S12,  S13,   e,   S15)
) :-
    energy_factor(A, F),
    dif(c, A),
    E is 6 * F + E0.

% moving 15 to 1
step(E0 - map(e, e,    e,    e,    e,    S6, S7,
                    S8,   S9,  S10,   e,
                   S12,  S13,  S14,   A),
      E - map(A, e,    e,    e,    e,    S6, S7,
                    S8,   S9,  S10,   e,
                   S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 10 * F + E0.

% moving 15 to 2
step(E0 - map(S1, e,    e,    e,    e,    S6, S7,
                    S8,   S9,  S10,   e,
                   S12,  S13,  S14,   A),
      E - map(S1, A,    e,    e,    e,    S6, S7,
                    S8,   S9,  S10,   e,
                   S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 9 * F + E0.

% moving 15 to 3
step(E0 - map(S1, S2,    e,    e,    e,    S6, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   A),
      E - map(S1, S2,    A,    e,    e,    S6, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 7 * F + E0.

% moving 15 to 4
step(E0 - map(S1, S2,   S3,    e,    e,    S6, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   A),
      E - map(S1, S2,   S3,    A,    e,    S6, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 5 * F + E0.

% moving 15 to 5
step(E0 - map(S1, S2,   S3,   S4,    e,    S6, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   A),
      E - map(S1, S2,   S3,   S4,    A,    S6, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 3 * F + E0.

% moving 15 to 6
step(E0 - map(S1, S2,   S3,   S4,   S5,  e, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   A),
      E - map(S1, S2,   S3,   S4,   S5,  A, S7,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 3 * F + E0.

% moving 15 to 7
step(E0 - map(S1, S2,   S3,   S4,   S5,  e, e,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   A),
      E - map(S1, S2,   S3,   S4,   S5,  e, A,
                     S8,   S9,  S10,   e,
                    S12,  S13,  S14,   e)
) :-
    energy_factor(A, F),
    dif(d, A),
    E is 3 * F + E0.

% moving 8 to 1
step(E0 - map(e, e,     S3,     S4,    S5,    S6, S7,
                      A,    S9,    S10,   S11,
                    S12,   S13,    S14,   S15),
      E - map(A, e,     S3,     S4,    S5,    S6, S7,
                      e,    S9,    S10,   S11,
                    S12,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 3 * F + E0.

% moving 9 to 1
step(E0 - map(e, e,     e,    S4,    S5,    S6, S7,
                    S8,     A,    S10,   S11,
                   S12,   S13,    S14,   S15),
      E - map(A, e,     e,    S4,    S5,    S6, S7,
                    S8,     e,    S10,   S11,
                   S12,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 5 * F + E0.

% moving 10 to 1
step(E0 - map(e, e,     e,    e,    S5,    S6, S7,
                    S8,    S9,    A,   S11,
                   S12,   S13,  S14,   S15),
      E - map(A, e,     e,    e,    S5,    S6, S7,
                    S8,    S9,    e,   S11,
                   S12,   S13,  S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 7 * F + E0.

% moving 11 to 1
step(E0 - map(e, e,     e,    e,    e,    S6, S7,
                    S8,    S9,  S10,    A,
                   S12,   S13,  S14,  S15),
      E - map(A, e,     e,    e,    e,    S6, S7,
                    S8,    S9,   S10,   e,
                   S12,   S13,   S14, S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 9 * F + E0.

% moving 8 to 2
step(E0 - map(S1, e,     S3,     S4,    S5,    S6, S7,
                       A,    S9,    S10,   S11,
                     S12,   S13,    S14,   S15),
      E - map(S1, A,     S3,     S4,    S5,    S6, S7,
                       e,    S9,    S10,   S11,
                     S12,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 2 * F + E0.

% moving 9 to 2
step(E0 - map(S1, e,      e,     S4,    S5,    S6, S7,
                      S8,     A,    S10,   S11,
                     S12,   S13,    S14,   S15),
      E - map(S1, A,      e,     S4,    S5,    S6, S7,
                      S8,     e,    S10,   S11,
                     S12,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 4 * F + E0.

% moving 10 to 2
step(E0 - map(S1, e,      e,     e,    S5,    S6, S7,
                      S8,    S9,     A,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, A,      e,     e,    S5,    S6, S7,
                      S8,    S9,     e,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 6 * F + E0.

% moving 11 to 2
step(E0 - map(S1, e,      e,     e,    e,    S6, S7,
                      S8,    S9,   S10,    A,
                     S12,   S13,   S14,  S15),
      E - map(S1, A,      e,     e,    e,    S6, S7,
                      S8,    S9,   S10,    e,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 8 * F + E0.

% moving 8 to 3
step(E0 - map(S1, S2,     e,     S4,    S5,    S6, S7,
                       A,    S9,    S10,   S11,
                     S12,   S13,    S14,   S15),
      E - map(S1, S2,     A,     S4,    S5,    S6, S7,
                       e,    S9,    S10,   S11,
                     S12,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 2 * F + E0.

% moving 9 to 3
step(E0 - map(S1, S2,     e,     S4,    S5,    S6, S7,
                      S8,     A,    S10,   S11,
                     S12,   S13,    S14,   S15),
      E - map(S1, S2,     A,     S4,    S5,    S6, S7,
                      S8,     e,    S10,   S11,
                     S12,   S13,    S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 2 * F + E0.

% moving 10 to 3
step(E0 - map(S1, S2,     e,     e,    S5,    S6, S7,
                      S8,    S9,     A,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,     A,     e,    S5,    S6, S7,
                      S8,    S9,     e,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 4 * F + E0.

% moving 11 to 3
step(E0 - map(S1, S2,     e,     e,    e,    S6, S7,
                      S8,    S9,   S10,    A,
                     S12,   S13,   S14,  S15),
      E - map(S1, S2,     A,     e,    e,    S6, S7,
                      S8,    S9,   S10,    e,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 6 * F + E0.

% moving 8 to 4
step(E0 - map(S1, S2,     e,     e,    S5,    S6, S7,
                       A,    S9,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,     e,     A,    S5,    S6, S7,
                       e,    S9,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 4 * F + E0.

% moving 9 to 4
step(E0 - map(S1, S2,    S3,     e,    S5,    S6, S7,
                      S8,     A,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,     A,    S5,    S6, S7,
                      S8,     e,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 2 * F + E0.

% moving 10 to 4
step(E0 - map(S1, S2,    S3,     e,    S5,    S6, S7,
                      S8,    S9,     A,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,     A,    S5,    S6, S7,
                      S8,    S9,     e,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 2 * F + E0.

% moving 11 to 4
step(E0 - map(S1, S2,    S3,     e,    e,    S6, S7,
                      S8,    S9,   S10,    A,
                     S12,   S13,   S14,  S15),
      E - map(S1, S2,    S3,     A,    e,    S6, S7,
                      S8,    S9,   S10,    e,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 4 * F + E0.

% moving 8 to 5
step(E0 - map(S1, S2,    e,    e,    e,    S6, S7,
                       A,   S9,  S10,  S11,
                     S12,  S13,  S14,  S15),
      E - map(S1, S2,    e,    e,    A,    S6, S7,
                       e,   S9,  S10,  S11,
                     S12,  S13,  S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 6 * F + E0.

% moving 9 to 5
step(E0 - map(S1, S2,    S3,     e,     e,    S6, S7,
                      S8,     A,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,     e,     A,    S6, S7,
                      S8,     e,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 4 * F + E0.

% moving 10 to 5
step(E0 - map(S1, S2,    S3,    S4,     e,    S6, S7,
                      S8,    S9,     A,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,    S4,     A,    S6, S7,
                      S8,    S9,     e,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 2 * F + E0.

% moving 11 to 5
step(E0 - map(S1, S2,    S3,    S4,    e,    S6, S7,
                      S8,    S9,   S10,    A,
                     S12,   S13,   S14,  S15),
      E - map(S1, S2,    S3,    S4,    A,    S6, S7,
                      S8,    S9,   S10,    e,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 2 * F + E0.

% moving 8 to 6
step(E0 - map(S1, S2,     e,     e,     e,     e, S7,
                       A,    S9,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,     e,     e,     e,     A, S7,
                       e,    S9,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 8 * F + E0.

% moving 9 to 6
step(E0 - map(S1, S2,    S3,     e,    e,    e, S7,
                      S8,     A,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,     e,    e,    A, S7,
                      S8,     e,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 6 * F + E0.

% moving 10 to 6
step(E0 - map(S1, S2,    S3,    S4,    e,    e, S7,
                      S8,    S9,     A,  S11,
                     S12,   S13,   S14,  S15),
      E - map(S1, S2,    S3,    S4,    e,    A, S7,
                      S8,    S9,     e,  S11,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 4 * F + E0.

% moving 11 to 6
step(E0 - map(S1, S2,    S3,    S4,    S5,    e, S7,
                      S8,    S9,   S10,     A,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,    S4,    S5,    A, S7,
                      S8,    S9,   S10,    e,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 2 * F + E0.

% moving 8 to 7
step(E0 - map(S1, S2,     e,     e,     e,     e, e,
                       A,    S9,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,     e,     e,     e,     e, A,
                       e,    S9,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(a, A) ; dif(a, S12) ),
    E is 9 * F + E0.

% moving 9 to 7
step(E0 - map(S1, S2,    S3,     e,    e,    e, e,
                      S8,     A,   S10,   S11,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,     e,    e,    e, A,
                      S8,     e,   S10,   S11,
                     S12,   S13,   S14,   S15)
) :-
    energy_factor(A, F),
    once( dif(b, A) ; dif(b, S13) ),
    E is 7 * F + E0.

% moving 10 to 7
step(E0 - map(S1, S2,    S3,    S4,    e,    e, e,
                      S8,    S9,     A,  S11,
                     S12,   S13,   S14,  S15),
      E - map(S1, S2,    S3,    S4,    e,    e, A,
                      S8,    S9,     e,  S11,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(c, A) ; dif(c, S14) ),
    E is 5 * F + E0.

% moving 11 to 7
step(E0 - map(S1, S2,    S3,    S4,    S5,    e, e,
                      S8,    S9,   S10,     A,
                     S12,   S13,   S14,   S15),
      E - map(S1, S2,    S3,    S4,    S5,    e, A,
                      S8,    S9,   S10,    e,
                     S12,   S13,   S14,  S15)
) :-
    energy_factor(A, F),
    once( dif(d, A) ; dif(d, S15) ),
    E is 3 * F + E0.

search(Steps0, Result) :-
    get_from_heap(Steps0, Energy0, Map0, Steps1),
    (   Map0 == map(e, e, e, e, e, e, e,
                         a, b, c, d,
                         a, b, c, d)
    ->  Result = Energy0
    ;   (   bagof(Step, step(Energy0 - Map0, Step), Steps2)
        ->  list_to_heap(Steps2, Steps3),
            merge_heaps(Steps1, Steps3, Steps)
        ;   Steps = Steps1
        ),
        search(Steps, Result)
    ).

part_one(Map, Result) :-
    singleton_heap(Steps, 0, Map),
    search(Steps, Result).

part_two(Input, Result) :-
    Input = Result.

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne).

main([]) :- main(['input.txt']).
