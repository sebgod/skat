%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_skat.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun 01 Jun 10:38:01 CEST 2014
%
%----------------------------------------------------------------------------%

:- module test_skat.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- import_module list.
:- import_module pretty_printer.
:- import_module skat.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

main(!IO) :-
    io.write_line("TODO: tests", !IO).

%----------------------------------------------------------------------------%
:- end_module test_skat.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
