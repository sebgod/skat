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

:- import_module skat.deck.

%----------------------------------------------------------------------------%

:- type eval
    ---> eval(
            jacks     :: int
         ).

:- func evaluate(deck) = eval.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module skat.card.
:- import_module skat.rank.
:- import_module skat.suit.
:- import_module int.

%----------------------------------------------------------------------------%

evaluate(Cards) = eval(straight_of(Cards, jack)).

%----------------------------------------------------------------------------%
:- end_module skat.eval.
%----------------------------------------------------------------------------%
