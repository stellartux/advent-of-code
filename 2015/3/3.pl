#!/usr/bin/env swipl

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics), [blanks//0]).
:- use_module(library(ordsets)).

visit_houses(Visited) -->
    visit_houses((0, 0), [(0, 0)], Visited), !.

visit_houses((X0, Y0), Visited0, Visited) -->
    (   "<", { X #= X0 - 1, Y #= Y0 }
    |   ">", { X #= X0 + 1, Y #= Y0 }
    |   "^", { X #= X0, Y #= Y0 - 1 }
    |   "v", { X #= X0, Y #= Y0 + 1 }
    ),
    { ord_add_element(Visited0, (X, Y), Visited1) },
    visit_houses((X, Y), Visited1, Visited).

visit_houses(_, Visited, Visited, "", "").

:- begin_tests(visit_houses).
:- set_prolog_flag(double_quotes, codes).

    test(example1) :-
        phrase(visit_houses(Set), ">"),
        length(Set, 2).
    test(example2) :-
        phrase(visit_houses(Set), "^>v<"),
        length(Set, 4).
    test(example3) :-
        phrase(visit_houses(Set), "^v^v^v^v^v"),
        length(Set, 2).

:- end_tests(visit_houses).

zip([], [], [], []) :- !.
zip([L], [], [L], []) :- !.
zip([L|Ls], [R|Rs]) --> [L], [R], !, zip(Ls, Rs).

main([]) :- main("input.txt").
main([Filename]) :-
    phrase_from_file(visit_houses(PartOne), Filename),
    length(PartOne, CountOne),
    writeln(CountOne),
    phrase_from_file(zip(HumanSanta, RobotSanta), Filename),
    phrase(visit_houses(HumanHouses), HumanSanta),
    phrase(visit_houses(RobotHouses), RobotSanta),
    ord_union(HumanHouses, RobotHouses, PartTwo),
    length(PartTwo, CountTwo),
    writeln(CountTwo).
