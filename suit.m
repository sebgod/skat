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

:- import_module coloured_pretty_printer.
:- import_module enum.
:- import_module list.

%----------------------------------------------------------------------------%

:- type suit
    ---> diamonds
    ;    hearts
    ;    spades
    ;    clubs.

:- instance enum(suit).

:- type suits.

:- func (suit ^ suit_symbol) = string.

:- func (suit ^ suit_colour) = ansi_colour.

:- func (suit ^ suit_value) = int.

:- func from_list(list(suit)) = suits.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module pretty_printer.
:- import_module require.

%----------------------------------------------------------------------------%
%
% Suit is an instance of enum. The int value is the base value of the colour.
%

:- instance enum(suit) where [
    (from_int(Value) = Suit  :- suit_value(Suit, Value)),
    (to_int(Suit)    = Suit ^ suit_value)
].

:- type card_map == map(suit, int).
:- type suit_card == pair(suit, int).
:- type suits ---> suits(card_map).

from_list(List) = suits(Map) :-
    from_list2(List, init, Map).

:- pred from_list2(list(suit), card_map, card_map).
:- mode from_list2(in, in, out) is det.

from_list2([], !Map).
from_list2([Suit | Suits], !Map) :-
    ( transform_value(
        pred(Value::in, ValueN::out) is det :- ValueN = Value + 1,
        Suit,
        !Map
      )
    ->
        true
    ;
        det_insert(Suit, 1, !Map)
    ),
    from_list2(Suits, !Map).

Suit ^ suit_value = Value :- suit_value(Suit, Value).

:- pred suit_value(suit, int).
:- mode suit_value(in, out) is det.
:- mode suit_value(out, in) is semidet.

suit_value(diamonds,  9).
suit_value(hearts,   10).
suit_value(spades,   11).
suit_value(clubs,    12).

%----------------------------------------------------------------------------%
%
% Pretty printing
%

Suit ^ suit_symbol = Symbol :- suit_symbol(Suit, Symbol).

:- pred suit_symbol(suit, string).
:- mode suit_symbol(in, out) is det.
:- mode suit_symbol(out, in) is semidet.

suit_symbol(diamonds, "♦").
suit_symbol(hearts,   "♥").
suit_symbol(spades,   "♠").
suit_symbol(clubs,    "♣").


Suit ^ suit_colour = Colour :- suit_colour(Suit, Colour).

:- pred suit_colour(suit, ansi_colour).
:- mode suit_colour(in, out) is det.
:- mode suit_colour(out, in) is semidet.

suit_colour(diamonds, yellow).
suit_colour(hearts,   red).
suit_colour(spades,   green).
suit_colour(clubs,    black).

:- func suit_to_doc(suit) = doc.

suit_to_doc(Suit) =
    colour_on_black(ansi(Suit^suit_colour, normal), str(Suit^suit_symbol)).

:- func suits_to_doc(suits) = doc.

suits_to_doc(suits(Map)) =
    group(map(suit_card_to_doc, to_assoc_list(Map))).

:- func suit_card_to_doc(suit_card) = doc.

suit_card_to_doc(Suit-Count) = group([format(Count), format(Suit)]).

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    update_formatters(
        [
            fmt($module, "suit", 0, fmt_any(suit_to_doc)),
            fmt($module, "suits", 0, fmt_any(suits_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module skat.suit.
%----------------------------------------------------------------------------%
