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

:- module(pivot_chords, [pivot_chords/2]).

:- use_module(harmony(chords)).

pivot_chords(KeyChain, Chords) :-
    pivot_chords(KeyChain, Chords, []).

pivot_chords([Key1, Key2|RemainingChain], [Chord|RemainingChords], UsedNotes) :-
    pivot_chord(Key1, Key2, Chord, Notes),
    \+ member(Notes, UsedNotes),
    pivot_chords([Key2|RemainingChain], RemainingChords, [Notes|UsedNotes]).

pivot_chords([_], [], _).

pivot_chord(Key1, Key2, pivot(chord(Key1, RNA1, Notes), chord(Key2, RNA2, Notes)), Notes) :-
    chords:chord_of_key(Key1, RNA1, Notes),
    chords:chord_of_key(Key2, RNA2, Notes).

% vim: ft=prolog
