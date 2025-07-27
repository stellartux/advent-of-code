#!/usr/bin/env swipl
% usage: swipl 4.pl FILENAME
% https://adventofcode.com/2020/day/4

:- module(aoc2020day4, []).
:- module(aoc2020day4).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).
load(Result) --> sequence(string, "\n\n", Result), eol.

pairs(Pairs) --> sequence(pair, blank, Pairs).
pair(A - B)  --> [C0, C1, C2], { atom_chars(A, [C0, C1, C2]) }, ":", nonblanks(B0),
{atom_codes(B, B0)}.

valid_pairs(Line) :-
    once(phrase(pairs(Pairs), Line)),
    pairs_keys(Pairs, Keys0),
    exclude(=(cid), Keys0, Keys),
    msort(Keys, [byr, ecl, eyr, hcl, hgt, iyr, pid]).

part_one(Lines, Result) :-
    aggregate_all(count, (
        member(Line, Lines),
        once(phrase(pairs(Pairs), Line)),
        pairs_keys(Pairs, Keys0),
        exclude(=(cid), Keys0, Keys),
        sort(Keys, [byr, ecl, eyr, hcl, hgt, iyr, pid])
    ), Result).

valid_passport(Line) :-
    once(phrase(passport(Pairs), Line)),
    pairs_keys(Pairs, Keys0),
    exclude(=(cid), Keys0, Keys),
    sort(Keys, [byr, ecl, eyr, hcl, hgt, iyr, pid]).

passport(Pairs) --> sequence(field, blank, Pairs), eol.

field(Pair) -->
    birth_year(Pair) | issue_year(Pair) | expiration_year(Pair) | height(Pair) |
    hair_color(Pair) | eye_color(Pair) | passport_id(Pair) | country_id(Pair).

birth_year(byr - BYR)         --> "byr:", integer(BYR), { between(1920, 2002, BYR) }.
issue_year(iyr - IYR)         --> "iyr:", integer(IYR), { between(2010, 2020, IYR) }.
expiration_year(eyr - EYR)    --> "eyr:", integer(EYR), { between(2020, 2030, EYR) }.
height(hgt - cm(CM))          --> "hgt:", integer(CM), "cm", { between(150, 193, CM) }.
height(hgt - in(IN))          --> "hgt:", integer(IN), "in", { between(59, 76, IN) }.
hair_color(hcl - Digits)      --> "hcl:#", { length(Digits, 6) }, xdigits(Digits).
eye_color(ecl - Color)        --> "ecl:", color(Color).
passport_id(pid - PassportID) --> "pid:", { length(PassportID, 9) }, digits(PassportID).
country_id(cid - CountryID)   --> "cid:", nonblanks(CountryID).

color(amb) --> "amb".
color(blu) --> "blu".
color(brn) --> "brn".
color(gry) --> "gry".
color(grn) --> "grn".
color(hzl) --> "hzl".
color(oth) --> "oth".

part_two --> include(valid_passport), length.

main([File]) :-
    load(File, Input),
    part_one(Input, ResultOne),
    writeln(ResultOne),
    part_two(Input, ResultTwo),
    writeln(ResultTwo).
