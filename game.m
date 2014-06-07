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
:- import_module list.
:- import_module random.

%----------------------------------------------------------------------------%

:- type game.

:- type phase
    ---> init(deck)
    ;    dealt(
            player_cards :: list(deck),
            skat         :: deck
         ).

:- inst phase_init  --->  init(ground).
:- inst phase_init_unique == unique(init(ground)).
:- inst phase_dealt ---> dealt(ground, ground).
:- inst phase_dealt_unique == unique(dealt(ground, ground)).

:- mode game_init_in == in(unique(game(phase_init))).
:- mode game_init_ui == in(unique(game(phase_init_unique))).
:- mode game_init_uo == out(unique(game(phase_init_unique))).

:- mode game_dealt_in == in(unique(game(phase_dealt))).
:- mode game_dealt_ui == in(unique(game(phase_dealt_unique))).
:- mode game_dealt_uo == out(unique(game(phase_dealt_unique))).

:- func init = game.
:- mode init = game_init_uo is det.

:- pred deal(game, game, supply, supply).
:- mode deal(game_init_in, game_dealt_uo, mdi, muo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module skat.card.

%----------------------------------------------------------------------------%

:- type game
    ---> game(phase).

init = game(init(deck_all)).

deal(game(init(!.Deck)), game(dealt(Players, Skat)), !Random) :-
    P1 = det_draw_cards(10, !.Deck, !:Deck, !.Random, !:Random),
    P2 = det_draw_cards(10, !.Deck, !:Deck, !.Random, !:Random),
    P3 = det_draw_cards(10, !.Deck, Skat, !.Random, !:Random),
    Players = [P1, P2, P3].

%----------------------------------------------------------------------------%
:- end_module skat.game.
%----------------------------------------------------------------------------%
