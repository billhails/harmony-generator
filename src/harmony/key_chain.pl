% Harmony Tools
%
% Copyright (C) 2018  Bill Hails
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
