%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: player.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sat  7 Jun 21:51:11 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module skat.player.

:- interface.

:- import_module list.
:- import_module skat.deck.

%----------------------------------------------------------------------------%

:- type player
    ---> player(
            player_cards  :: deck,
            player_role   :: role
         ).

:- type players == list(player).

:- type role
    ---> first
    ;    middle
    ;    last.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

% TODO: include/import/use modules

%----------------------------------------------------------------------------%

% TODO: implement predicates & functions

%----------------------------------------------------------------------------%
:- end_module skat.player.
%----------------------------------------------------------------------------%
