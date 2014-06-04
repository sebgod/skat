%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: rank.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon  2 Jun 21:12:58 CEST 2014
%
%----------------------------------------------------------------------------%

:- module skat.rank.

:- interface.

%----------------------------------------------------------------------------%

:- type rank
    ---> jack
    ;    ace
    ;    ten
    ;    king
    ;    queen
    ;    nine
    ;    eight
    ;    seven.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module skat.rank.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%