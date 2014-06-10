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
            stats_suits      :: suits,
            stats_values     :: suits,
            stats_high_cards :: suits
         ).

%:- type spec
%    ----> bid

:- type evals == list(eval).

:- func evaluate_for_bidding(player) = eval.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module skat.card.
:- import_module skat.deck.
:- import_module skat.rank.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

evaluate_for_bidding(Player) = eval(Stats) :-
    Deck = Player^player_cards,
    Jacks = Deck `straight_by_rank` jack,
    Highs =
    ( Jacks = 4 ->
        deck_empty
    ; Jacks = -4 ->
        deck_empty
    ;
        cards_by_rank(Deck, jack)
    ),
    Stats = stats(Jacks,
                  Deck^deck_suits,
                  Deck^deck_suit_values,
                  Highs^deck_suits).

%----------------------------------------------------------------------------%
:- end_module skat.eval.
%----------------------------------------------------------------------------%
