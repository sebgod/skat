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

:- import_module enum.
:- import_module skat.rank.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type card.

:- func card_rank(card) = rank.

:- func card_suit(card) = suit.

:- func of(rank, suit) = card.

:- instance enum(card).

:- func det_from_int(int) = card.

:- func to_offset(card) = int.

:- func number_of_cards = int.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- use_module char.
:- import_module coloured_pretty_printer.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pretty_printer.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%
%
% The card type is a packed int consisting of the rank index and the
% suit index.
% XXX Since Mercury guarantees that sizeof(int) >= 4, this aproach works well
% enough, but is unfortuantely not generic enough for games like Poker.
% This could be handled by the access functions though (and using a native
% type)
%

:- type card
    ---> card(int).

:- instance enum(card) where [
    (to_int(card(Index)) = Index),
    (func(from_int/1) is card_from_int)
].

:- func card_from_int(int) = card.
:- mode card_from_int(in) = out is semidet.

card_from_int(Index) = card(Index) :-
        Index >= 0,
        Index < number_of_cards.

det_from_int(Index) = Card :-
    (
        Card0 = card_from_int(Index)
    ->
        Card = Card0
    ;
        unexpected($file, $pred,
            format("card_index %x is not in range [0..31]", [i(Index)]))
    ).

to_offset(Card) = 1 << to_int(Card).

card_rank(card(Index)) = Rank :-
    (
        Rank0^rank_index =  Index /\ 0b111
    ->
        Rank = Rank0
    ;
        unexpected($file, $pred,
            format("card_index %x is not a valid rank", [i(Index)]))
    ).

card_suit(card(Index)) = Suit :-
    (
        Suit0^suit_index = (Index >> 3) /\ 0b11
    ->
        Suit = Suit0
    ;
        unexpected($file, $pred,
            format("card_index %x is not a valid suit", [i(Index)]))
    ).

of(Rank, Suit) = card((Suit^suit_index << 3) \/ Rank^rank_index).

:- func rank_index(rank) = int.
:- mode rank_index(in)   = out is det.
:- mode rank_index(out)  = in  is semidet.

rank_index(ace)   = 0.
rank_index(ten)   = 1.
rank_index(king)  = 2.
rank_index(queen) = 3.
rank_index(jack)  = 4.
rank_index(nine)  = 5.
rank_index(eight) = 6.
rank_index(seven) = 7.

:- func suit_index(suit) = int.
:- mode suit_index(in)   = out is det.
:- mode suit_index(out)  = in  is semidet.

suit_index(clubs)    = 0.
suit_index(spades)   = 1.
suit_index(hearts)   = 2.
suit_index(diamonds) = 3.

number_of_cards = 32.

%----------------------------------------------------------------------------%
%
% Pretty printing
%

:- func card_to_doc(card) = doc.

card_to_doc(Card) =
    docs([format(Card^card_suit), format(Card^card_rank)]).

:- func card_to_doc_unicode_coloured(card) = doc.

card_to_doc_unicode_coloured(Card) = colour_on_black(Colour, CardDoc) :-
    RankOffset = rank_offset(Card^card_rank),
    Suit = Card^card_suit,
    Colour = ansi(Suit^suit_colour, normal),
    CardChar = char.det_from_int(0x1f000 + RankOffset + Suit^suit_offset),
    CardDoc = str(char_to_string(CardChar) ++ " ").

:- func rank_offset(rank) = int.

rank_offset(ace)   = 0x1.
rank_offset(seven) = 0x7.
rank_offset(eight) = 0x8.
rank_offset(nine)  = 0x9.
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
    io.get_environment_var("SKAT_FORMAT_OP", FormatOp, !IO),
    CardToDoc =
    ( FormatOp = yes("unicol") ->
        card_to_doc_unicode_coloured
    ;
        card_to_doc
    ),
    update_formatters([fmt($module, "card", 0, fmt_any(CardToDoc))], !IO).
%----------------------------------------------------------------------------%
:- end_module skat.card.
%----------------------------------------------------------------------------%
