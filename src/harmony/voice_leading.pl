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

% IN: [
%     chord(key(c, major), i_major, [0, 4, 7]),
%     pivot(
%         chord(key(c, major), neapolitan_sixth, [1, 5, 8]),
%         chord(key(c_sh, major), i_major, [1, 5, 8])
%     ),
%     ...
% ]
%
% OUT: [
%     voicing(key(c, major)-i_major, [S, A, T, B]),
%     voicing([key(c, major)-neapolitan_sixth, key(c_sh, major)-i_major], [S, A, T, B]),
%     ...
% ]

:- module(voice_leading, [voice_leading/2]).

:- use_module(harmony(midi)).
:- use_module(harmony(chords)).
:- use_module(library(clpfd)).

voice_leading(Chords, Voicings) :-
    convert_to_voicings(Chords, V),
    arrange(V, Voicings).

convert_to_voicings([], []).

convert_to_voicings([chord(Key, RNA, _)|RemainingChords],
                    [voicing(Key-RNA)|RemainingVoicings]) :-
    convert_to_voicings(RemainingChords, RemainingVoicings).

convert_to_voicings([pivot(chord(Key1, RNA1, _), chord(Key2, RNA2, _))|RemainingChords],
                    [voicing([Key1-RNA1, Key2-RNA2])|RemainingVoicings) :-
    convert_to_voicings(RemainingChords, RemainingVoicings).

arrange([Voicing|RemainingVoicings], [Arranged|RemainingArranged]) :-
    arrange_first(Voicing, Arranged),
    arrange_rest(RemainingVoicings, [Arranged|RemainingArranged]).

arrange_first(voicing(Key-RNA), voicing(Key-RNA, [S, A, T, B])) :-
    arrange_first_chord(Key, RNA, [S, A, T, B])).

arrange_first(voicing([Key1-RNA1, Key2-Rna2]), voicing([Key1-RNA1, Key2-Rna2], [S, A, T, B])) :-
    arrange_first_chord(Key1, RNA1, [S, A, T, B])).

arrange_first_chord(Key, RNA, [S, A, T, B])) :-
    root_of_chord_by_rna(Key, RNA, Root),
    mode_of_chord(RNA, Type),
    choose_pitch(bass, Root, Type, [], B),
    choose_pitch(tenor, Root, Type, [B], T),
    choose_pitch(alto, Root, Type, [T, B], A),
    choose_pitch(soprano, Root, Type, [A, T, B], S).

choose_pitch(Voice, Root, ChordType, Below, Pitch) :-
    notes_of_chord(Root, ChordType, Notes),
    member(Note, Notes),
    length(Notes, ChordSize),
    midi_pitches(UsedList, Below),
    list_to_set(UsedList, Used),
    distinct_if_possible(Note, ChordSize, Used),
    midi_pitch(Note, Pitch),
    above_all(Pitch, Below),
    pitch_in_range(Voice, Pitch).

distinct_if_possible(Note, 4, Used) :-
    !,
    \+ member(Note, Used).

distinct_if_possible(_, 3, Used) :-
    length(Used, 3), !.

distinct_if_possible(Note, 3, Used) :-
    \+ member(Note, Used).

above_all(_, []).

above_all(Pitch, [P|Pitches]) :-
    Pitch #>= P,
    above_all(Pitch, Pitches).

arrange_rest([], [_]).

arrange_rest([ToVoice|RemainingVoicings], [AlreadyArranged,Arranged|RemainingArrangements]) :-
    arrange_next(AlreadyArranged, ToVoice, Arranged),
    arrange_rest(RemainingVoicings, [Arranged|RemainingArrangements]).

arrange_next(
             voicing(Key-_, [S, A, T, B]), % already voiced
             voicing(Key-RNA), % to be voiced
             voicing(Key-RNA, [S1, A1, T1, B1])
             ) :- 
    arrange_next_chord(Key-RNA, [S, A, T, B], [S1, A1, T1, B1]).

arrange_next(
             voicing([_, Key-_], [S, A, T, B]), % already voiced
             voicing(Key-RNA2), % to be voiced
             voicing(Key-RNA2, [S1, A1, T1, B1])
             ) :- 
    arrange_next_chord(Key-RNA2, [S, A, T, B], [S1, A1, T1, B1]).

arrange_next(
             voicing(Key1-_, [S, A, T, B]), % already voiced
             voicing([Key1-RNA1, Key2-RNA2]), % to be voiced
             voicing([Key1-RNA1, Key2-RNA2], [S1, A1, T1, B1])
             ) :- 
    arrange_next_chord([Key1-RNA1, Key2-RNA2], [S, A, T, B], [S1, A1, T1, B1]).

arrange_next(
             voicing([_, Key1-_], [S, A, T, B]), % already voiced
             voicing([Key1-RNA1, Key2-RNA2]), % to be voiced
             voicing([Key1-RNA1, Key2-RNA2], [S1, A1, T1, B1])
             ) :- 
    arrange_next_chord([Key1-RNA1, Key2-RNA2], [S, A, T, B], [S1, A1, T1, B1]).

arrange_next_chord(Key-RNA, [S, A, T, B], [S1, A1, T1, B1]):-
    % if RNA is Neap then 3rd in bass.
    % otherwise nearest note to previous bass
    % keep notes in common
    % otherwise move to nearest note in opposition to the bass
    % check notes in range
    % check for parallel fifths/octaves

% vim: ft=prolog
