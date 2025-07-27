#!/usr/bin/env swipl
% usage: swipl 11.pl FILENAME
% https://adventofcode.com/2020/day/11

:- module(aoc2020day11, []).
:- module(aoc2020day11).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module('helpers/helpers.pl', [
    fixed_point/3,
    map_grid0/3,
    von_neumann_neighbourhood/2,
    yxth0/3
]).

load(File, Grid) :- once(phrase_from_file(load(Grid), File)).
load(Grid) --> sequence(row, Grid).
row(Row)   --> nonblanks(Row0), eol, { Row0 \= [], maplist(char_code, Row, Row0) }.

step(Grid0, Grid) :- map_grid0(step(Grid0), Grid0, Grid).

step(_, _, '.', '.') :- !.
step(Grid, Here, Chair0, Chair) :-
    aggregate_all(count, (
        von_neumann_neighbourhood(Here, There),
        yxth0(There, Grid, '#')
    ), Neighbours),
    (   Chair0 = 'L'
    ->  ( Neighbours = 0 -> Chair = '#' ; Chair = 'L' )
    ;   ( Neighbours < 4 -> Chair = '#' ; Chair = 'L' )
    ).

display(Grid) :- maplist([R] >> format("~s~n", [R]), Grid).

% 92,857,373 inferences, 8.394 CPU in 8.416 seconds (100% CPU, 11062052 Lips)
part_one(Grid0, Result) :-
    fixed_point(step, Grid0, Grid),
    aggregate_all(count, yxth0(_, Grid, '#'), Result).

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne).
