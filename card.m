%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: card.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon  2 Jun 21:18:58 CEST 2014
%
%----------------------------------------------------------------------------%

:- module skat.card.

:- interface.

:- import_module skat.rank.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type card
    ---> card(
            card_rank :: rank,
            card_suit :: suit
         ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module coloured_pretty_printer.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module require.
:- import_module std_util.

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func card_to_doc(suit) = doc.

card_to_doc(_Card) = str("[]").

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters(
        [
            fmt($module, "card", 0, fmt_any(card_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module skat.card.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
