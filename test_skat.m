%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_skat.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun  1 Jun 10:38:01 CEST 2014
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
:- import_module pair.
:- import_module pretty_printer.
:- use_module random.
:- import_module require.
:- import_module skat.
:- import_module skat.deck.
:- import_module skat.suit.
:- import_module skat.rank.
:- import_module skat.card.

%----------------------------------------------------------------------------%

main(!IO) :-
    random.init(2312384324, Supply),
    print_test("diamonds", diamonds, !IO),
    print_test("hearts",   hearts,   !IO),
    print_test("spades",   spades,   !IO),
    print_test("clubs",    clubs,    !IO),
    print_test("ace of spades", card(ace, spades), !IO),
    print_test("queen of hearts", card(queen, hearts), !IO),
    print_test("all cards", deck_all, !IO),
    print_test("no cards", deck_empty, !IO),
    (
        Drawn = draw_card(deck_all, AllMinusOne, Supply, _)
    ->
        print_test("drawn card", card(Drawn), !IO),
        print_test("left in deck", AllMinusOne, !IO)
    ;
        unexpected($file, $pred, "draw_card/5 should not have failed!")
    ).

:- pred print_test(string::in, T::in, io::di, io::uo) is det.

print_test(Name, Entity, !IO) :-
    io.print(Name, !IO),
    io.print(" = ", !IO),
    write_doc(format(Entity), !IO),
    io.nl(!IO).

%----------------------------------------------------------------------------%
:- end_module test_skat.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
