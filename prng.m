%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: prng.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sat  7 Jun 22:27:19 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Encapsulation of a PRNG for card drawing and AI decisions.
%----------------------------------------------------------------------------%

:- module skat.prng.

:- interface.

:- import_module skat.card.

%----------------------------------------------------------------------------%

:- type prng.

:- func init_determ = prng.
:- mode init_determ = uo is det.

:- pred next_card(card, prng, prng).
:- mode next_card(out, in, out) is det.
:- mode next_card(out, mdi, muo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module random.

%----------------------------------------------------------------------------%

:- type prng == supply.

init_determ = Prng :-
    random.init(231238, Prng).

next_card(Card, !Prng) :-
    random(0, number_of_cards, CardIndex, !Prng),
    Card = det_from_int(CardIndex).

%----------------------------------------------------------------------------%
:- end_module skat.prng.
%----------------------------------------------------------------------------%
