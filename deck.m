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

:- import_module list.
:- import_module skat.card.

%----------------------------------------------------------------------------%

:- type deck.

:- func all_cards = deck.
:- mode all_cards = out is det.
:- mode all_cards = in  is semidet.

:- func no_cards = deck.
:- mode no_cards = out is det.
:- mode no_cards = in  is semidet.

:- func to_list(deck) = list(card).

:- func draw_card(deck) = card is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module tree_bitset.

:- type deck == tree_bitset(int).

all_cards = !:AllCards :-
    !:AllCards = no_cards,
    insert_list(0 `..` 31, !AllCards).

no_cards = Empty :- empty(Empty).

to_list(Deck) = (func(Int) = card(Int)) `map` to_sorted_list(Deck).

draw_card(Deck) = Card :-
    [Card | _] = to_list(Deck).

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module skat.deck.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
