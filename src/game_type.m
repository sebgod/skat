%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
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

:- import_module enum.
:- import_module maybe.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

:- type game_type
    ---> game_type(
            game_base       :: base,
            game_factor     :: maybe(int),
            game_announced  :: announced_grade
         ).

:- type base
    ---> colour(suit)
    ;    grand
    ;    null.

:- instance enum(base).

:- type announced_grade
    ---> playing
    ;    hand
    ;    schneider
    ;    schwarz
    ;    ouvert
    ;    null_hand_ouvert.

:- func game_value(game_type) = int.

:- func announced_grade_to_factor(announced_grade) = int.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
%
% Base factor is an enum(T) instance
%

:- instance enum(base) where [
    ( to_int(Base) =
        ( Base = grand ->
            24
        ; Base = null  ->
            23
        ; Base = colour(Suit) ->
            Suit^suit_value
        ;
            unexpected($file, $pred, "Unknown basic game type")
        )
    ),
    ( from_int(Value) =
        ( Value = 24 ->
            grand
        ; Value = 23 ->
            null
        ;
            colour(from_int(Value))
        )
    )
].


game_value(GameType) = Value :-
    Base   = GameType^game_base,
    Factor = GameType^game_factor,
    Grade  = GameType^game_announced,
    Value =
    ( Factor = yes(Tips)
    ->
        to_int(Base) * (Tips + announced_grade_to_factor(Grade))
    ;
        Base = null,
        Factor = no
    ->
        ( Grade = playing ->
            23
        ; Grade = hand ->
            35
        ; Grade = ouvert ->
            46
        ; Grade = null_hand_ouvert ->
            59
        ;
            unexpected($file, $pred,
                "valid null types: null, hand, overt, hand+overt")
        )
    ;
        unexpected($file, $pred, "unknown game type")
    ).

announced_grade_to_factor(Grade) =
    ( Grade = playing ->
        1
    ; Grade = hand ->
        2
    ; Grade = schneider ->
        3
    ; Grade = schwarz ->
        4
    ; Grade = ouvert ->
        5
    ;
        unexpected($file, $pred, "null games have no factor")
    ).

%----------------------------------------------------------------------------%
:- end_module skat.game_type.
%----------------------------------------------------------------------------%
