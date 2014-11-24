%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
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

:- import_module pretty_printer.

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

:- func rank_value(rank) = int.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- func rank_to_doc(rank) = doc.

:- implementation.

:- import_module char.
:- import_module coloured_pretty_printer.
:- import_module io.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

rank_value(Rank) = Value :- rank_value(Rank, Value).

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

rank_to_doc(Rank) = str(from_char(Symbol)) :-
    rank_symbol(Rank, Symbol).

:- pred rank_symbol(rank, char).
:- mode rank_symbol(in, out) is det.
:- mode rank_symbol(out, in) is semidet.

rank_symbol(jack,  'J').
rank_symbol(ace,   'A').
rank_symbol(ten,   'T').
rank_symbol(king,  'K').
rank_symbol(queen, 'Q').
rank_symbol(nine,  '9').
rank_symbol(eight, '8').
rank_symbol(seven, '7').

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters(
        [
            fmt($module, "rank", 0, fmt_any(rank_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module skat.rank.
%----------------------------------------------------------------------------%
