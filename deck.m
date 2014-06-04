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

:- func all_cards = deck.
:- mode all_cards = out is det.
:- mode all_cards = in  is semidet.

:- func no_cards = deck.
:- mode no_cards = out is det.
:- mode no_cards = in  is semidet.


:- pred contains_card(deck, T)  <= (card(T), enum(T)).
:- mode contains_card(in, in) is semidet.

:- some [T] pred member_card(T, deck) => (card(T), enum(T)).
:- mode member_card(out, in) is nondet.

:- some[T] func draw_card(deck, supply, supply) = T => (card(T), enum(T)).
:- mode draw_card(in, di, uo) = out is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module coloured_pretty_printer.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module tree_bitset.
:- import_module univ.

%----------------------------------------------------------------------------%

:- type deck
    ---> deck(tree_bitset(int)).

all_cards = deck(!:AllCards) :-
    deck(!:AllCards) = no_cards,
    insert_list(0 `..` 31, !AllCards).

no_cards = deck(Empty) :- empty(Empty).

contains_card(deck(Deck), Card) :- contains(Deck, index(Card)).

member_card(Card, deck(Deck)) :- tree_bitset.member(Card, Deck).

draw_card(Deck, !Supply) = Card :-
    Deck \= no_cards,
    [Card | _] = to_list(Deck).

%----------------------------------------------------------------------------%
%
% Module private auxilary functions
%

:- some [T] func to_list(deck) = list(T) => (card(T), enum(T)).

to_list(deck(Deck)) = to_sorted_list(Deck).

:- func to_fat_list(deck) = list(card).

to_fat_list(deck(Deck)) = index_to_card `map` to_sorted_list(Deck).

:- func index_to_card(int) = card.

index_to_card(Index) = card(Index).

%----------------------------------------------------------------------------%
%
% Pretty printing
%
:- func deck_to_doc(deck) = doc.

deck_to_doc(Deck) = group(format `map` to_fat_list(Deck)).
%    format_list(map_to_univ(to_fat_list(Deck)), empty_str).

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
