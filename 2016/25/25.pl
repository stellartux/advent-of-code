#!/usr/bin/env swipl
% usage: swipl 25.pl [FILENAME]

:- module(aoc2016day25, []).
:- module(aoc2016day25).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(instruction, Result), eol, eos.

instruction(cpy(X, Y)) --> "cpy ", ( register(X) | integer(X) ), " ", register(Y), eol.
instruction(inc(X))    --> "inc ", register(X), eol.
instruction(dec(X))    --> "dec ", register(X), eol.
instruction(jnz(X, Y)) --> "jnz ", ( register(X) | integer(X) ), " ", integer(Y), eol.
instruction(out(X))    --> "out ", register(X), eol.

register(a) --> "a".
register(b) --> "b".
register(c) --> "c".
register(d) --> "d".

graphviz(Instructions, Graph) :-
    length(Instructions, Len),
    numlist(1, Len, Nums),
    maplist([N, I, N - I] >> true, Nums, Instructions, Pairs),
    once(phrase(graphviz(Pairs), Graph)).

graphviz(Pairs) -->
    "digraph {", eol,
    sequence(gv, Pairs),
    "}", eol, eos.

head(N) --> "    N", integer(N), " [label=\"".
tail(N) --> ")\"]", eol, { succ(N, N1) }, "    N", integer(N), " -> N", integer(N1).

gv(N - cpy(X, Y)) --> head(N), "cpy(", ( register(X) | integer(X) ), ", ", register(Y), tail(N), eol.
gv(N - inc(X))    --> head(N), "inc(", register(X), tail(N), eol.
gv(N - dec(X))    --> head(N), "dec(", register(X), tail(N), eol.
gv(N - out(X))    --> head(N), "out(", register(X), tail(N), eol.
gv(N - jnz(X, Y)) --> head(N), "jnz(", ( register(X) | integer(X) ), ")\"]", eol,
    (   { integer(X), X \== 0 }
    ->  []
    ;   { succ(N, N1) },
        "    N", integer(N), " -> N", integer(N1), " [label=\"zero\"]" , eol
    ),
    (   { integer(X), X == 0 }
    ->  []
    ;   { Z is N + Y },
        "    N", integer(N), " -> N", integer(Z), " [label=\"non-zero\"]", eol
    ).

main([File]) :-
    once(phrase_from_file(load(Input), File)),
    graphviz(Input, String),
    format("~s", [String]).
