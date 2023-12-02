#!/usr/bin/env swipl
% usage: swipl 2.pl FILENAME

:- module(aoc2023day2, []).
:- initialization(main, main).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

line(Game-Bags) --> "Game ", integer(Game), ": ", sequence(bag, `; `, Bags), eol.

bag(RGB) --> sequence(field, `, `, RGBs), { foldl(sum_fields, RGBs, 0-0-0, RGB) }.

field(R-0-0) --> integer(R), " red".
field(0-G-0) --> integer(G), " green".
field(0-0-B) --> integer(B), " blue".

sum_fields(R0-G0-B0, R1-G1-B1, R-G-B) :-
    R #= R1 + R0,
    G #= G1 + G0,
    B #= B1 + B0.

possible_round(R-G-B) :-
    R in 0..12, G in 0..13, B in 0..14.

possible_game(_-RGBs) :-
    maplist(possible_round, RGBs).

part_one(Games, Result) :-
    include(possible_game, Games, PossibleGames),
    pairs_keys(PossibleGames, PossibleIds),
    sum_list(PossibleIds, Result).

power(R-G-B, Power) :-
    Power #= R * G * B.

cubes_max(R0-G0-B0, R1-G1-B1, R-G-B) :-
    R #= max(R1, R0),
    G #= max(G1, G0),
    B #= max(B1, B0).

fewest_possible_cubes(_-[Game|Games], Result) :-
    foldl(cubes_max, Games, Game, Result).

part_two(Games, Result) :-
    maplist(fewest_possible_cubes, Games, MinRGBs),
    maplist(power, MinRGBs, Powers),
    sum_list(Powers, Result).

main([File]) :-
    once(phrase_from_file(sequence(line, Games), File)),
    part_one(Games, ResultOne),
    part_two(Games, ResultTwo),
    writeln(ResultOne),
    writeln(ResultTwo).

main([]) :-
    main(["input.txt"]).
