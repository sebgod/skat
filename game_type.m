%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: game_type.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun  8 Jun 23:26:50 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module skat.game_type.

:- interface.

:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type game_type
    ---> game_type(
            game_base    :: base,
            game_factor  :: factor,
            game_grade   :: grade
         ).

:- type base
    ---> colour(suit)
    ;    grand
    ;    null.

:- type factor
    ---> tips(int)
    ;    jacks(int)
    ;    null.

:- type grade
    ---> playing
    ;    hand
    ;    schneider_declared
    ;    schneider_played
    ;    schwarz_declared
    ;    schwarz_played
    ;    overt
    ;    null_hand_overt.

% TODO: insert predicates & functions

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

% TODO: include/import/use modules

%----------------------------------------------------------------------------%

% TODO: implement predicates & functions

%----------------------------------------------------------------------------%
:- end_module skat.game_type.
%----------------------------------------------------------------------------%
