#!/usr/bin/env swipl
% usage: swipl 13.pl FILENAME
% https://adventofcode.com/2020/day/13

:- module(aoc2020day13, []).
:- module(aoc2020day13).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Input) :- once(phrase_from_file(load(Input), Filename)).

load((DepartureTime, BusIDs)) -->
    integer(DepartureTime), eol,
    sequence(bus_id, ",", BusIDs), eol, eol.

bus_id(0)  --> "x".
bus_id(ID) --> integer(ID).

part_one((DepartureTime, BusIDs), Result) :-
    Time in DepartureTime..10000000000000000,
    BusID #> 0,
    list_to_fdset(BusIDs, BusIDSet),
    BusID in_set BusIDSet,
    Time mod BusID #= 0,
    Result #= BusID * (Time - DepartureTime),
    once(labeling([min(Time)], [BusID, Time, Result])).


main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne).
