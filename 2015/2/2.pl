#!/usr/bin/env swipl

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), [integer//1, blanks//0]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- initialization(main, main).

area(Area) -->
    integer(Length), "x", integer(Width), "x", integer(Height),
    {
        Sides = [Length * Width, Width * Height, Height * Length],
        sum_list(Sides, SumSides),
        min_list(Sides, MinSide),
        Area #= 2 * SumSides + MinSide
    },
    blanks.

:- begin_tests(area).
    :- set_prolog_flag(double_quotes, codes).
    test(example1) :-
        phrase(area(Area), "2x3x4"),
        assertion(Area #= 58).
    test(example2) :-
        phrase(area(Area), "1x1x10"),
        assertion(Area #= 43).
:- end_tests(area).

ribbon_length(RibbonLength) -->
    integer(Length), "x", integer(Width), "x", integer(Height),
    {
        msort([Length, Width, Height], [Small, Medium, _]),
        RibbonLength #= 2 * (Small + Medium) + Length * Width * Height
    },
    blanks.

:- begin_tests(ribbon_length).
    :- set_prolog_flag(double_quotes, codes).
    test(example1) :-
        phrase(ribbon_length(RibbonLength), "2x3x4"),
        assertion(RibbonLength #= 34).
    test(example2) :-
        phrase(ribbon_length(RibbonLength), "1x1x10"),
        assertion(RibbonLength #= 14).
:- end_tests(ribbon_length).

solve_for(DCG, Filename) :-
    once(phrase_from_file(sequence(DCG, Values), Filename)),
    sum_list(Values, Total),
    writeln(Total).

main([]) :-
    main(["input.txt"]).

main([Filename]) :-
    solve_for(area, Filename),
    solve_for(ribbon_length, Filename).
