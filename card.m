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

:- type card ---> card(rank, suit).

:- typeclass card(T).

:- func (T ^ card_rank) = rank <= card(T).

:- func (T ^ card_suit) = suit <= card(T).

:- instance card(card).
:- instance card(int).

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
% card typeclass
%

:- typeclass card(T) where [
    func card(T) = card
].

(Card ^ card_rank) = Rank :- card(Rank, _Suit) = card(Card).

(Card ^ card_suit) = Suit :- card(_Rank, Suit) = card(Card).

:- instance card(card) where [
    (card(Card) = Card)
].

:- instance card(int) where [
    (card(CodedCard) =
        ( suit_index(Suit) = CodedCard /\ 0b11,
          rank_index(Rank) = (CodedCard >> 0b11) /\ 0b1111
        ->
            card(Rank, Suit)
        ;
            unexpected($file, $pred,
                format("card_index %d is invalid", [i(CodedCard)]))
        )
    )
].

:- func rank_index(rank) = int.
:- mode rank_index(in) = out is det.
:- mode rank_index(out) = in is semidet.

rank_index(ace)   = 0.
rank_index(ten)   = 1.
rank_index(king)  = 2.
rank_index(queen) = 3.
rank_index(jack)  = 4.
rank_index(nine)  = 5.
rank_index(eight) = 6.
rank_index(seven) = 7.

:- func suit_index(suit) = int.
:- mode suit_index(in) = out is det.
:- mode suit_index(out) = in is semidet.

suit_index(clubs)    = 0.
suit_index(spades)   = 1.
suit_index(hearts)   = 2.
suit_index(diamonds) = 3.

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func card_to_doc(card) = doc.

card_to_doc(card(Rank, Suit)) = colour_on_black(Colour, CardDoc) :-
    Colour = ansi(Suit^suit_colour, normal),
    CardChar = det_from_int(0x1f000 + rank_offset(Rank) + suit_offset(Suit)),
    CardDoc = str(char_to_string(CardChar)).

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
