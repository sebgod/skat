%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
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
:- import_module skat.deck.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type eval
    ---> eval(
            jacks     :: int,
            suits     :: suits
         ).

:- type evals == list(eval).

:- func evaluate_for_bidding(deck) = eval.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module skat.card.
:- import_module skat.rank.
:- import_module skat.suit.
:- import_module int.

%----------------------------------------------------------------------------%

evaluate_for_bidding(Cards) =
    eval(straight_of(Cards, jack), Cards^deck_suits).

%----------------------------------------------------------------------------%
:- end_module skat.eval.
%----------------------------------------------------------------------------%
