%----------------------------------------------------------------------------%
% vim: tw=78 ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: eval.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sat  7 Jun 23:45:07 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% The eval module is for evaluation of cards.
%----------------------------------------------------------------------------%

:- module skat.eval.

:- interface.

:- import_module list.
:- import_module skat.player.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type eval
    ---> eval(
            eval_stats  :: stats
            %eval_spec   :: spec
         ).

:- type stats
    ---> stats(
            stats_jacks      :: int,
            stats_suits      :: suit_cardinalities,
            stats_values     :: suit_cardinalities,
            stats_high_cards :: suit_cardinalities
         ).

%:- type spec
%    ----> bid

:- type evals == list(eval).

:- func evaluate_for_bidding(player) = eval.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module sparse_bitset.
:- import_module skat.card.
:- import_module skat.deck.
:- import_module skat.rank.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type suit_set == sparse_bitset(suit).

:- func all_suits = suit_set.

all_suits = list_to_set([diamonds, hearts, spades, clubs]).

    % TODO: Lacks implementation
evaluate_for_bidding(Player) = eval(Stats) :-
    Deck = Player^player_cards,
    Jacks = Deck `straight_by_rank` jack,
    NonTrumps = [jack, ace, ten, king, queen, nine, eight, seven],
    ( Jacks = 4 ->
        foldl2(count_succ(Deck), NonTrumps, deck_empty, Highs, all_suits, _)
    ; Jacks = -4 ->
        Highs = deck_empty
    ;
        Highs = deck_empty
    ),
    Stats = stats(Jacks,
                  Deck^deck_suit_cardinalities,
                  Deck^deck_suit_values,
                  Highs^deck_suit_cardinalities).

:- pred count_succ(deck::in, rank::in, deck::in, deck::out,
    suit_set::in, suit_set::out) is det.

count_succ(Deck, Rank, !Straight, !Suits) :-
    ByRank = Deck `straight_by_rank` Rank.

%----------------------------------------------------------------------------%
:- end_module skat.eval.
%----------------------------------------------------------------------------%
