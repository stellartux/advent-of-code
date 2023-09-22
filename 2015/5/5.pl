#!/usr/bin/env swipl

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(pcre)).
:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(re_compile, true).

is_nice1(String) :-
    re_match("[aeiou].*[aeiou].*[aeiou]", String),
    re_match("(.)\\1", String),
    \+ re_match("ab|cd|pq|xy", String).

:- begin_tests(is_nice1).

    test(example1)         :- is_nice1("ugknbfddgicrmopn").
    test(example2)         :- is_nice1("aaa").
    test(example3, [fail]) :- is_nice1("yzbqklnj").
    test(example4, [fail]) :- is_nice1("haegwjzuvuyypxyu").
    test(example5, [fail]) :- is_nice1("dvszwmarrgswjxmb").

:- end_tests(is_nice1).

is_nice2(String) :-
    re_match("(..).*\\1", String),
    re_match("(.).\\1", String).

:- begin_tests(is_nice2).

    test(example1)         :- is_nice2("qjhvhtzxzqqjkmpb").
    test(example2)         :- is_nice2("xxyxx").
    test(example3, [fail]) :- is_nice2("uurcxstgmygtbstg").
    test(example4, [fail]) :- is_nice2("ieodomkazucvgmuy").

:- end_tests(is_nice2).

count_nice(Nice1, Nice2) -->
    string_without("\n", String),
    { length(String, Count), Count #> 0 },
    blanks, !,
    {   is_nice1(String)
    ->  Nice1 #= Nice1_ + 1
    ;   Nice1 #= Nice1_
    },
    {   is_nice2(String)
    ->  Nice2 #= Nice2_ + 1
    ;   Nice2 #= Nice2_
    },
    count_nice(Nice1_, Nice2_).

count_nice(0, 0, "", "") :- !.

:- initialization(main, main).
main([]) :- main(["input.txt"]).
main([File]) :-
    phrase_from_file(count_nice(Count1, Count2), File),
    writeln(Count1),
    writeln(Count2).
