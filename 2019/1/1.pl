#!/usr/bin/env swipl

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

integer_nl(N) --> integer(N), "\n".

mass_fuel(Mass, Fuel) :-
    Fuel #= Mass div 3 - 2.

mass_fuel_r(Mass, Fuel) :-
    mass_fuel_r(Mass, 0, Fuel).

mass_fuel_r(Mass, Fuel, Fuel) :- Mass #< 9, !.
mass_fuel_r(Mass, FuelIn, FuelOut) :-
    mass_fuel(Mass, Fuel0),
    Fuel1 #= Fuel0 + FuelIn,
    mass_fuel_r(Fuel0, Fuel1, FuelOut).

main([]) :- main(["2019/1/input.txt"]).
main([File]) :-
    phrase_from_file(sequence(integer_nl, Masses), File), !,
    maplist(mass_fuel, Masses, Fuels),
    sum_list(Fuels, PartOne),
    writeln(PartOne),
    maplist(mass_fuel_r, Fuels, FuelFuels),
    sum_list(FuelFuels, ExtraFuel),
    PartTwo #= PartOne + ExtraFuel,
    writeln(PartTwo).
