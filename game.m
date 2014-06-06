%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: game.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri  6 Jun 22:57:03 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% The game module contains the game adt, a complete representation of the
% current game state.
%----------------------------------------------------------------------------%

:- module skat.game.

:- interface.

:- import_module skat.deck.

%----------------------------------------------------------------------------%

:- type game.

:- type phase
    ---> init(stack :: deck).

:- inst phase_init_unique == unique(init(ground)).

:- mode game_init == out(unique(game(phase_init_unique))).

:- func init = game.
:- mode init = game_init is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- type game
    ---> game(phase).

init = game(init(deck_all)).

%----------------------------------------------------------------------------%
:- end_module skat.game.
%----------------------------------------------------------------------------%
