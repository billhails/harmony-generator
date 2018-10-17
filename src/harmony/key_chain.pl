:- module(key_chain, [key_chain/1]).

:- use_module(harmony(note)).

key_chain(Chain) :-
    key_chain([c, g, d, a, e, b, f_sh, c_sh, g_sh, e_fl, b_fl, f],
              [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
              Chain).

key_chain([], [], _).

key_chain(AvailableKeys, AvailableIntervals, [key(KeyNote, Mode)|[key(PrevKeyNote, PrevMode)|Chain]]) :-
    member(KeyNote, AvailableKeys),
    note:note(Offset, KeyNote),
    note:note(PrevOffset, PrevKeyNote),
    interval(Offset, PrevOffset, Interval),
    member(Interval, AvailableIntervals),
    delete(AvailableKeys, KeyNote, RemainingKeys),
    delete(AvailableIntervals, Interval, RemainingIntervals),
    random_mode(Mode),
    key_chain(RemainingKeys, RemainingIntervals, [key(PrevKeyNote, PrevMode)|Chain]).

key_chain(AvailableKeys, AvailableIntervals, [key(KeyNote, Mode)]) :-
    member(KeyNote, AvailableKeys),
    delete(AvailableKeys, KeyNote, RemainingKeys),
    random_mode(Mode),
    key_chain(RemainingKeys, AvailableIntervals, []).

interval(Offset1, Offset2, Interval) :-
    Difference is Offset2 - Offset1,
    positive_interval(Difference, Interval).

positive_interval(Difference, Difference) :-
    abs(Difference, Difference), !.

positive_interval(Difference, Interval) :-
    Interval is Difference + 12.

random_mode(Mode) :-
    random_member(Mode, [major, minor]).

% vim: ft=prolog
