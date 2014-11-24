%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
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

:- import_module deconstruct.
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
:- import_module stream.string_writer.
:- import_module string.
:- import_module string.builder.

%----------------------------------------------------------------------------%

:- type sb == string.builder.state.

main(!IO) :-
    real_main(!IO, init_determ, _, string.builder.init, SB),
    fprint(to_string(SB), !IO).

:- pred real_main(io::di, io::uo, prng::mdi, prng::muo, sb::di, sb::uo) is det.

real_main(!IO, !Supply, !SB) :-
    get_default_formatter_map(Formatter, !IO),
    get_default_params(Params, !IO),
    DocWriter = (pred(Doc::in, !.SB::di, !:SB::uo) is det :-
        write_doc_to_stream(string.builder.handle, canonicalize,
            Formatter,
            Params^pp_line_width, Params^pp_max_lines, Params^pp_limit,
            Doc, !SB)
    ),
    Test = print_test(DocWriter),
    Test(format(diamonds), "diamonds", !SB),
    Test(format(hearts),   "hearts",   !SB),
    Test(format(spades),   "spades",   !SB),
    Test(format(clubs),    "clubs",    !SB),
    Test(format(ace `of` spades), "ace of spades", !SB),
    Test(format(queen `of` hearts), "queen of hearts", !SB),
    Test(format(deck_all), "all cards", !SB),
    Test(format(cards_by_rank(deck_all, jack)), "all jacks", !SB),
    Test(format(deck_all^deck_suit_cardinalities),
        "all cards except jack by suit", !SB),
    Test(format(straight_by_rank(deck_all, jack)), "with all jacks", !SB),
    Test(format(straight_by_rank(deck_empty, jack)), "without all jacks", !SB),
    Test(format(deck_empty), "no cards",  !SB),
    (
        Drawn = draw_card(deck_all, AllMinusOne, !.Supply, !:Supply)
    ->
        Test(format(Drawn), "drawn card", !SB),
        Test(format(AllMinusOne), "left in deck", !SB),
        ( contains_card(AllMinusOne, Drawn) ->
            unexpected($file, $pred, "the drawn card should not be a member")
        ;
            true
        )
    ;
        unexpected($file, $pred, "draw_card/4 should not have failed!")
    ),
    Test(format(game.init), "game#init", !SB),
    deal(game.init, Dealt, !Supply),
    Test(format(Dealt), "game#dealt", !SB).

:- type doc_writer_pred == pred(doc, sb, sb).
:- inst doc_writer_pred == (pred(in, di, uo) is det).

:- pred print_test(doc_writer_pred::in(doc_writer_pred), doc::in, string::in,
    sb::di, sb::uo) is det.

print_test(DocWriter, Doc, Name, !SB) :-
    format(string.builder.handle, "%s = ", [s(Name)], !SB),
    DocWriter(Doc, !SB),
    format(string.builder.handle, "\n", [], !SB).

:- pred fprint(string::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%
% fprint in C is just calling fprintf with stdout as the FILE*
% fprint in C# is just calling System.Console.Write
% fprint in Java is just calling System.out.print
%

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pragma foreign_proc("C", fprint(Text::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    fprintf(stdout, ""%s"", Text);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C#", fprint(Text::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    System.Console.Write(Text);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("Java", fprint(Text::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    System.out.print(Text);
").

%----------------------------------------------------------------------------%
:- end_module test_skat.
%----------------------------------------------------------------------------%
