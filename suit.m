%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: suit.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun  1 Jun 10:34:53 CEST 2014
%
%----------------------------------------------------------------------------%

:- module skat.suit.

:- interface.

:- import_module pretty_printer.

%----------------------------------------------------------------------------%


:- type suit
    ---> diamonds
    ;    hearts
    ;    spades
    ;    clubs.

:- func suit_to_doc(suit) = doc.


%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module coloured_pretty_printer.
:- import_module io.
:- import_module list.
:- import_module require.
:- import_module std_util.

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func (suit ^ suit_symbol) = string.

Suit ^ suit_symbol = Symbol :- suit_symbol(Suit, Symbol).

:- pred suit_symbol(suit, string).
:- mode suit_symbol(in, out) is det.
:- mode suit_symbol(out, in) is semidet.

suit_symbol(diamonds, "♦").
suit_symbol(hearts,   "♥").
suit_symbol(spades,   "♠").
suit_symbol(clubs,    "♣").

:- func (suit ^ suit_colour) = ansi_colour.

Suit ^ suit_colour = Colour :- suit_colour(Suit, Colour).

:- pred suit_colour(suit, ansi_colour).
:- mode suit_colour(in, out) is det.
:- mode suit_colour(out, in) is semidet.

suit_colour(diamonds, yellow).
suit_colour(hearts,   red).
suit_colour(spades,   green).
suit_colour(clubs,    black).

suit_to_doc(Suit) = Doc :-
    SuitFgAndBg = compose(
        fg(ansi(Suit^suit_colour, normal)),
        ( if Suit^suit_colour = black then grey_bg else black_bg )
    ),
    Doc = SuitFgAndBg(str(Suit^suit_symbol)).


:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters(
        [
            fmt($module, "suit", 0, fmt_any(suit_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module skat.suit.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
