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

:- import_module char.
:- import_module coloured_pretty_printer.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func card_to_doc(card) = doc.

card_to_doc(Card) = colour_on_black(Colour, str(char_to_string(Symbol))) :-
    Suit = Card^card_suit,
    Colour = ansi(Suit^suit_colour, normal),
    Symbol = det_from_int(0x1f000 +
        rank_offset(Card^card_rank) + suit_offset(Suit)).

:- func rank_offset(rank) = int.

rank_offset(ace)   = 1.
rank_offset(seven) = 7.
rank_offset(eight) = 8.
rank_offset(nine)  = 9.
rank_offset(ten)   = 0xa.
rank_offset(jack)  = 0xb.
rank_offset(queen) = 0xd.
rank_offset(king)  = 0xe.

:- func suit_offset(suit) = int.

suit_offset(spades)   = 0xa0.
suit_offset(hearts)   = 0xb0.
suit_offset(diamonds) = 0xc0.
suit_offset(clubs)    = 0xd0.

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
