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

:- import_module random.
:- import_module skat.card.

%----------------------------------------------------------------------------%

:- type deck.

:- func deck_all = deck.
:- mode deck_all = out is det.
:- mode deck_all = in  is semidet.

:- func deck_empty = deck.
:- mode deck_empty = out is det.
:- mode deck_empty = in  is semidet.


:- pred contains_card(deck, card).
:- mode contains_card(in, in) is semidet.

:- pred member_card(card, deck).
:- mode member_card(in, in) is semidet.
:- mode member_card(out, in) is nondet.

:- func draw_card(deck, deck, supply, supply) = card.
:- mode draw_card(in, out, mdi, muo) = out is semidet.
:- mode draw_card(in, out, in, out)  = out is semidet.

:- func det_draw_card(deck, deck, supply, supply) = card.
:- mode det_draw_card(in, out, mdi, muo) = out is det.
:- mode det_draw_card(in, out, in, out)  = out is det.


%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module coloured_pretty_printer.
:- use_module enum.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module univ.
:- import_module require.

%----------------------------------------------------------------------------%

:- type deck ---> deck(int).

deck_all = deck(0xffffffff).

deck_empty = deck(0).

contains_card(deck(Cards), Card) :-
    Cards \= 0,
    0 \= Cards /\ (1 << enum.to_int(Card)).

member_card(Card, deck(Cards)) :-
    Card = det_from_int(Index),
    member_bit(Index, 0, Cards).

:- pred member_bit(int, int, int).
:- mode member_bit(out, in, in) is nondet.
:- mode member_bit(in, in, in) is semidet.

member_bit(Card, Index, Cards) :-
    Cards /\ 1 = 1,
    Card = number_of_cards - Index.

member_bit(Card, Index, Cards) :-
    Cards \= 0,
    Cards /\ 1 = 0,
    member_bit(Card, Index + 1, Cards >> 1).

draw_card(!Deck, !Supply) = Card :-
    !.Deck \= deck_empty,
    deck(Cards) = !.Deck,
    random(0, number_of_cards, CardIndex, !Supply),
    ( contains_card(!.Deck, det_from_int(CardIndex)) ->
        !:Deck = deck(Cards `xor` (1 << CardIndex)),
        Card = det_from_int(CardIndex)
    ;
        Card = draw_card(!.Deck, !:Deck, !.Supply, !:Supply)
    ).

det_draw_card(!Deck, !Supply) = Card :-
    (
        Card0 = draw_card(!.Deck, !:Deck, !.Supply, !:Supply)
    ->
        Card = Card0
    ;
        unexpected($file, $pred, "Cannot draw a card from an empty deck")
    ).

%----------------------------------------------------------------------------%
%
% Module private auxilary functions
%

:- func valid_card_indices = list(int).

valid_card_indices = 0 `..` (number_of_cards - 1).

:- func cards_in_deck(deck) = list(card).

cards_in_deck(Deck) =
    filter(contains_card(Deck), map(det_from_int, valid_card_indices)).

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func deck_to_doc(deck) = doc.

deck_to_doc(Deck) = group(map(format, cards_in_deck(Deck))).

:- initialise init_deck/2.

:- pred init_deck(io::di, io::uo) is det.

init_deck(!IO) :-
    update_formatters(
        [
            fmt($module, "deck", 0, fmt_any(deck_to_doc))
        ], !IO).

%----------------------------------------------------------------------------%
:- end_module skat.deck.
%----------------------------------------------------------------------------%
