%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: deck.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Jun  3 14:21:05 CEST 2014
%
%----------------------------------------------------------------------------%

:- module skat.deck.

:- interface.

:- import_module enum.
:- import_module random.
:- import_module skat.card.

%----------------------------------------------------------------------------%

:- type deck.

:- func deck_size = int.

:- func deck_all = deck.
:- mode deck_all = out is det.
:- mode deck_all = in  is semidet.

:- func deck_empty = deck.
:- mode deck_empty = out is det.
:- mode deck_empty = in  is semidet.


:- pred contains_card(deck, T)  <= (card(T), enum(T)).
:- mode contains_card(in, in) is semidet.

:- some [T] pred member_card(T, deck) => (card(T), enum(T)).
:- mode member_card(in, in) is cc_nondet.
:- mode member_card(out, in) is nondet.

:- some [T] func draw_card(deck, deck, supply, supply) = T => (card(T), enum(T)).
:- mode draw_card(in, out, mdi, muo) = out is semidet.
:- mode draw_card(in, out, in, out)  = out is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module coloured_pretty_printer.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module univ.
:- import_module require.
:- import_module std_util.

%----------------------------------------------------------------------------%

:- type deck ---> deck(int).

deck_size = 32.

deck_all = deck(0xffffffff).

deck_empty = deck(0).

contains_card(deck(Cards), Card) :-
    Cards \= 0,
    0 \= Cards /\ (1 << index(Card)).

member_card(Card, deck(Cards)) :-
    member_bit(Card, 0, Cards).

:- pred member_bit(int, int, int).
:- mode member_bit(out, in, in) is nondet.
:- mode member_bit(in, in, in) is semidet.

member_bit(Card, Index, Cards) :-
    Cards /\ 1 = 1,
    Card = deck_size - Index.

member_bit(Card, Index, Cards) :-
    Cards \= 0,
    Cards /\ 1 = 0,
    member_bit(Card, Index + 1, Cards >> 1).

draw_card(!Deck, !Supply) = CardIndex :-
    !.Deck \= deck_empty,
    deck(Cards) = !.Deck,
    random(0, deck_size, CardIndex, !Supply),
    ( contains_card(!.Deck, CardIndex) ->
        !:Deck = deck(Cards `xor` (1 << CardIndex))
    ;
        CardIndex = index(draw_card(!.Deck, !:Deck, !.Supply, !:Supply))
    ).

%----------------------------------------------------------------------------%
%
% Module private auxilary functions
%

:- func valid_card_indices = list(int).

valid_card_indices = 0 `..` (deck_size - 1).

:- func indices_in_deck(deck) = list(int). % <= (card(T), enum(T)).

indices_in_deck(Deck) =
    filter(contains_card(Deck), valid_card_indices).

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func deck_to_doc(deck) = doc.

deck_to_doc(Deck) = group(map(format `compose` card, indices_in_deck(Deck))).

:- initialise init_deck/2.

:- pred init_deck(io::di, io::uo) is det.

init_deck(!IO) :-
    update_formatters(
        [
            fmt($module, "deck", 0, fmt_any(deck_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module skat.deck.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
