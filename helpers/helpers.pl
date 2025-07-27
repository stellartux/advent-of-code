:- module(helpers, [
    fixate/3,
    fixed_point/3,
    load_coords/5,
    load_coords_from_file/4,
    map_grid0/3,
    map_grid1/3,
    maplist_with_nth0/3,
    maplist_with_nth1/3,
    moore_neighbourhood/2,
    rb_map_with_keys/3,
    von_neumann_neighbourhood/2,
    yxth0/3,
    yxth1/3
]).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

:- meta_predicate load_coords(3, 2, -).
%!  load_coords(:Accept, :Reject, -Result) is det.
%   Result is a list of pairs of yx(Y, X)-ArgumentOfAccept
load_coords(Accept, Reject, Result) --> load_coords_(0, 0, Accept, Reject, Result), !.

:- meta_predicate load_coords_from_file(3, 2, +, -).
%!  load_coords_from_file(:Accept, :Reject, ++Filename, -Result) is det.
load_coords_from_file(Accept, Reject, Filename, Result) :-
    phrase_from_file(load_coords(Accept, Reject, Result), Filename).

load_coords_(X, Y, Accept, Reject, Coords) --> eol, !,
    (   { X == 0 }
    ->  { Coords = [] }
    ;   { succ(Y, Y1) },
        load_coords_(0, Y1, Accept, Reject, Coords)
    ).

load_coords_(X, Y, Accept, Reject, Coords, List0, List) :-
    succ(X, X1),
    (   call_dcg(Reject, List0, List1), Coords0 = Coords
    ;   call(Accept, Value, List0, List1), Coords = [yx(Y, X) - Value | Coords0]
    ),
    load_coords_(X1, Y, Accept, Reject, Coords0, List1, List).

%!  moore_neighbourhood(+YX0, -YX) is multi.
%   Visits neighbours top-to-bottom, left-to-right, assuming topleft is yx(0, 0).
%   This is the default sort order for yx terms.
moore_neighbourhood(yx(Y0, X), yx(Y, X)) :- Y #= Y0 - 1.
moore_neighbourhood(yx(Y, X0), yx(Y, X)) :- X #= X0 - 1.
moore_neighbourhood(yx(Y, X0), yx(Y, X)) :- X #= X0 + 1.
moore_neighbourhood(yx(Y0, X), yx(Y, X)) :- Y #= Y0 + 1.

%!  moore_neighbourhood(+ZYX0, -ZYX) is multi.
%   Visits neighbours in the default sort order for zyx terms.
moore_neighbourhood(zyx(Z0, Y, X), zyx(Z, Y, X)) :- Z #= Z0 - 1.
moore_neighbourhood(zyx(Z, Y0, X), zyx(Z, Y, X)) :- Y #= Y0 - 1.
moore_neighbourhood(zyx(Z, Y, X0), zyx(Z, Y, X)) :- X #= X0 - 1.
moore_neighbourhood(zyx(Z, Y, X0), zyx(Z, Y, X)) :- X #= X0 + 1.
moore_neighbourhood(zyx(Z, Y0, X), zyx(Z, Y, X)) :- Y #= Y0 + 1.
moore_neighbourhood(zyx(Z0, Y, X), zyx(Z, Y, X)) :- Z #= Z0 + 1.

%!  von_neumann_neighbourhood(YX0, YX) is multi.
%   Visits neighbours top-to-bottom, left-to-right, assuming topleft is yx(0, 0).
%   This is the default sort order for yx terms.
von_neumann_neighbourhood(yx(Y0, X0), yx(Y, X)) :-
    succ(Y0, Y), ( succ(X, X0) ; X = X0 ; succ(X0, X) ).

von_neumann_neighbourhood(yx(Y, X0), yx(Y, X)) :- succ(X, X0) ; succ(X0, X).

von_neumann_neighbourhood(yx(Y0, X0), yx(Y, X)) :-
    succ(Y, Y0), ( succ(X, X0) ; X = X0 ; succ(X0, X) ).

von_neumann_neighbourhood(zyx(Z0, Y0, X0), zyx(Z, Y, X)) :-
    between(-1, 1, ZOff),
    Z #= Z0 + ZOff,
    between(-1, 1, YOff),
    Y #= Y0 + YOff,
    between(-1, 1, XOff),
    X #= X0 + XOff,
    once(( ZOff \== 0 ; YOff \== 0 ; XOff \== 0 )).

:- meta_predicate fixate(2, +, -).
fixate(Goal, S0, S) :-
    ( S0 = S ; call(Goal, S0, S1), S0 \= S1, fixate(Goal, S1, S) ).

:- meta_predicate fixed_point(2, +, -).
fixed_point(Goal, S0, S) :-
    call(Goal, S0, S1), ( S0 = S1 -> S = S1 ; fixed_point(Goal, S1, S) ).

%!  rb_map_with_keys(+Tree, :G, -NewTree) is semidet.
%
%   For all nodes Key in the tree Tree, if the value associated with key
%   Key is Val0 in tree Tree, and   if call(G,Val0,ValF) holds, then the
%   value  associated  with  Key  in   NewTree    is   ValF.   Fails  if
%   call(G,Key,Val0,ValF)  is  not  satisfiable  for all  Val0. If G  is
%   non-deterministic,  rb_map_with_keys/3  will   backtrack   over  all
%   possible values from call(G,Key,Val0,ValF). You should not depend on
%   the order of tree traversal (currently: key order).

:- meta_predicate rb_map_with_keys(+,3,-).

rb_map_with_keys(t(Nil,Tree),Goal,NewTree2) =>
    NewTree2 = t(Nil,NewTree),
    map_with_keys(Tree,Goal,NewTree,Nil).

map_with_keys(black('',_,_,''),_,Nil0,Nil) => Nil0 = Nil.
map_with_keys(red(L,K,V,R),Goal,NewTree,Nil) =>
    NewTree = red(NL,K,NV,NR),
    call(Goal,K,V,NV),
    map_with_keys(L,Goal,NL,Nil),
    map_with_keys(R,Goal,NR,Nil).
map_with_keys(black(L,K,V,R),Goal,NewTree,Nil) =>
    NewTree = black(NL,K,NV,NR),
    call(Goal,K,V,NV),
    map_with_keys(L,Goal,NL,Nil),
    map_with_keys(R,Goal,NR,Nil).

:- meta_predicate maplist_with_nth0(3, +, -).
maplist_with_nth0(Goal, List0, List) :-
    length(List0, Length),
    (   succ(LastIndex, Length)
    ->  numlist(0, LastIndex, N0s),
        maplist(Goal, N0s, List0, List)
    ;   List = []
    ).

:- meta_predicate maplist_with_nth1(3, +, -).
maplist_with_nth1(Goal, List0, List) :-
    length(List0, LastIndex),
    (   numlist(1, LastIndex, N0s)
    ->  maplist(Goal, N0s, List0, List)
    ;   List = []
    ).

:- meta_predicate map_grid0(3, +, -).
map_grid0(Goal, Grid0, Grid) :-
    maplist_with_nth0({Goal}/[Y, Row0, Row] >> (
        maplist_with_nth0({Goal, Y}/[X, T0, T] >> (
            call(Goal, yx(Y, X), T0, T)
        ), Row0, Row)
    ), Grid0, Grid).


:- meta_predicate map_grid1(3, +, -).
map_grid1(Goal, Grid0, Grid) :-
    maplist_with_nth1({Goal}/[Y, Row0, Row] >> (
        maplist_with_nth1({Goal, Y}/[X, T0, T] >> (
            call(Goal, yx(Y, X), T0, T)
        ), Row0, Row)
    ), Grid0, Grid).

%!  yxth0(-YX, +Grid, +Value) is nondet.
%!  yxth0(+YX, +Grid, -Value) is det.
yxth0(yx(Y, X)) --> nth0(Y), nth0(X).

%!  yxth1(-YX, +Grid, +Value) is nondet.
%!  yxth1(+YX, +Grid, -Value) is det.
yxth1(yx(Y, X)) --> nth1(Y), nth1(X).
