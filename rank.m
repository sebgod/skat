%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: rank.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon  2 Jun 21:12:58 CEST 2014
% Stability: medium
%----------------------------------------------------------------------------%
% TODO: Module documentation
%----------------------------------------------------------------------------%

:- module skat.rank.

:- interface.

%----------------------------------------------------------------------------%

:- type rank
    ---> jack
    ;    ace
    ;    ten
    ;    king
    ;    queen
    ;    nine
    ;    eight
    ;    seven.

:- func (rank ^ rank_value) = int.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

Rank ^ rank_value = Value :- rank_value(Rank, Value).

:- pred rank_value(rank, int).
:- mode rank_value(in, out) is det.
:- mode rank_value(out, in) is nondet.

rank_value(jack,  2).
rank_value(ace,  11).
rank_value(ten,  10).
rank_value(king,  4).
rank_value(queen, 3).
rank_value(nine,  0).
rank_value(eight, 0).
rank_value(seven, 0).

%----------------------------------------------------------------------------%
:- end_module skat.rank.
%----------------------------------------------------------------------------%
