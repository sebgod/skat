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
:- import_module require.
:- import_module skat.
:- import_module skat.card.
:- import_module skat.deck.
:- import_module skat.prng.
:- import_module skat.game.
:- import_module skat.rank.
:- import_module skat.suit.

%----------------------------------------------------------------------------%

main(!IO) :-
    real_main(!IO, init_determ, _).

:- pred real_main(io, io, prng, prng).
:- mode real_main(di, uo, mdi, muo).

real_main(!IO, !Supply) :-
    print_test("diamonds", diamonds, !IO),
    print_test("hearts",   hearts,   !IO),
    print_test("spades",   spades,   !IO),
    print_test("clubs",    clubs,    !IO),
    print_test("ace of spades",   ace   `of` spades, !IO),
    print_test("queen of hearts", queen `of` hearts, !IO),
    print_test("all cards", deck_all,   !IO),
    print_test("all jacks", cards_by_rank(deck_all, jack), !IO),
    print_test("all cards except jacks by suit", deck_all^deck_suits, !IO),
    print_test("with all jacks", straight_of(deck_all, jack), !IO),
    print_test("without all jacks", straight_of(deck_empty, jack), !IO),
    print_test("no cards",  deck_empty, !IO),
    (
        Drawn = draw_card(deck_all, AllMinusOne, !.Supply, !:Supply)
    ->
        print_test("drawn card", Drawn, !IO),
        print_test("left in deck", AllMinusOne, !IO),
        ( contains_card(AllMinusOne, Drawn) ->
            unexpected($file, $pred, "the drawn card should not be a member")
        ;
            true
        )
    ;
        unexpected($file, $pred, "draw_card/4 should not have failed!")
    ),
    print_test("game#init", game.init,  !IO),
    deal(game.init, Dealt, !Supply),
    print_test("game#dealt", Dealt, !IO).

:- pred print_test(string::in, T::in, io::di, io::uo) is det.

print_test(Name, Entity, !IO) :-
    io.print(Name, !IO),
    io.print(" = ", !IO),
    write_doc(format(Entity), !IO),
    io.nl(!IO).

%----------------------------------------------------------------------------%
:- end_module test_skat.
%----------------------------------------------------------------------------%
