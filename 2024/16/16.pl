#!/usr/bin/env swipl
% usage: swipl 16.pl [FILENAME]

:- module(aoc2024day16, []).
:- module(aoc2024day16).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(heaps)).

load(Grid) --> sequence(row, Grid), eos.
row(Row)   --> nonblanks(Row0), { Row0 \== [], maplist(char_code, Row, Row0) }, eol.

yxth(yx(Y, X), Grid, Value) :- nth1(Y, Grid, Row), nth1(X, Row, Value).

step1(Grid, s(Score0, [YX0 | YXs], yx(YD0, XD0)), s(Score, [YX, YX0 | YXs], yx(YD, XD))) :-
    YX0 = yx(Y0, X0),
    YX = yx(Y, X),
    (   Score is Score0 + 1,      YD0 = YD, XD0 = XD
    ;   Score is Score0 + 1001, ( XD = YD0, YD = XD0 ; XD is -YD0, YD is -XD0 )
    ),
    Y is Y0 + YD,
    X is X0 + XD,
    \+ yxth(YX, Grid, '#'),
    \+ memberchk(YX, YXs).

priority(yx(Y, X), State, Priority) :-
    position(yx(Y0, X0), State),
    arg(1, State, Score),
    (   ( Y == Y0 ; X == X0 )
    ->  Priority is Score + abs(Y - Y0) + abs(X - X0)
    ;   Priority is Score + abs(Y - Y0) + abs(X - X0) + 1000
    ).

visit(MinScore, Target, State, Heap0, Heap) :-
    priority(Target, State, Priority),
    (   ( var(MinScore) ; Priority =< MinScore )
    ->  add_to_heap(Heap0, Priority, State, Heap)
    ;   Heap0 = Heap
    ).

position(Position, s(_, [Position | _], _)).

search(Grid, Result, State) :-
    once(yxth(YX0, Grid, 'S')),
    singleton_heap(Heap0, 0, s(0, [YX0], yx(0, 1))),
    yxth(Target, Grid, 'E'),
    search(Grid, Target, Heap0, Result, State).

search(Grid, Target, Heap0, MinScore, State) :-
    get_from_heap(Heap0, _, State0, Heap1),
    (   position(Target, State0)
    ->  arg(1, State0, MinScore),
        ( State0 = State ; search(Grid, Target, Heap1, MinScore, State) )
    ;   findall(State1, step1(Grid, State0, State1), State1s),
        foldl(visit(MinScore, Target), State1s, Heap1, Heap),
        search(Grid, Target, Heap, MinScore, State)
    ).

part_one(Grid, Result) :- once(search(Grid, Result, _)).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    part_one(Input, ResultOne),
    writeln(ResultOne).

main([]) :- main(['input.txt']).
