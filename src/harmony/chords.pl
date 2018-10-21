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

:- module(chords, [chord_of_key/3, root_of_chord_by_rna/3, mode_of_chord/2]).

:- use_module(harmony(note)).

chord_of_key(key(KeyNote, major), RNA, Notes) :-
    chord_of_major_key(KeyNote, RNA, OffsetNotes),
    sort(OffsetNotes, Notes).

chord_of_key(key(KeyNote, minor), RNA, Notes) :-
    chord_of_minor_key(KeyNote, RNA, OffsetNotes),
    sort(OffsetNotes, Notes).

chord_of_major_key(KeyNote, RNA, Notes) :-
    note(Offset, KeyNote),
    chord_of_c_major(RNA , OriginalNotes),
    offset_chord(Offset, OriginalNotes, Notes).

chord_of_minor_key(KeyNote, RNA, Notes) :-
    note(Offset, KeyNote),
    chord_of_c_minor(RNA, OriginalNotes),
    offset_chord(Offset, OriginalNotes, Notes).

offset_chord(Offset, OriginalNotes, OffsetNotes) :-
    add_offset(Offset, OriginalNotes, OffsetNotes).

add_offset(_, [], []).

add_offset(Offset, [X|T], [Y|U]) :-
    Y is (X + Offset) mod 12,
    add_offset(Offset, T, U).

chord_of_c_major(i_major, [0, 4, 7]).
chord_of_c_major(neapolitan_sixth, [1, 5, 8]).
chord_of_c_major(ii_minor, [2, 5, 9]).
chord_of_c_major(iii_minor, [4, 7, 11]).
chord_of_c_major(iv_minor, [5, 8, 0]).
chord_of_c_major(iv_major, [5, 9, 0]).
chord_of_c_major(v_major, [7, 11, 2]).
chord_of_c_major(v_dominant_seventh, [7, 11, 2, 5]).
chord_of_c_major(italian_sixth, [8, 0, 6]).
chord_of_c_major(german_sixth, [8, 0, 3, 6]).
chord_of_c_major(french_sixth, [8, 0, 2, 6]).
chord_of_c_major(vi_minor, [9, 0, 4]).
chord_of_c_major(vii_diminished, [11, 2, 5]).
chord_of_c_major(vii_diminished_seventh, [11, 2, 5, 8]).

chord_of_c_minor(i_minor, [0, 3, 7]).
chord_of_c_minor(neapolitan_sixth, [1, 5, 8]).
chord_of_c_minor(ii_diminished, [2, 5, 8]).
chord_of_c_minor(ii_diminished_seventh, [2, 5, 8, 11]).
chord_of_c_minor(ii_minor, [2, 5, 9]).
chord_of_c_minor(iii_major, [3, 7, 10]).
chord_of_c_minor(iii_augmented, [3, 7, 11]).
chord_of_c_minor(iv_minor, [5, 8, 0]).
chord_of_c_minor(iv_major, [5, 9, 0]).
chord_of_c_minor(v_minor, [7, 10, 2]).
chord_of_c_minor(v_major, [7, 11, 2]).
chord_of_c_minor(v_dominant_seventh, [7, 11, 2, 5]).
chord_of_c_minor(italian_sixth, [8, 0, 6]).
chord_of_c_minor(german_sixth, [8, 0, 3, 6]).
chord_of_c_minor(french_sixth, [8, 0, 2, 6]).
chord_of_c_minor(vi_major, [8, 0, 3]).
chord_of_c_minor(vi_diminished, [9, 0, 3]).
chord_of_c_minor(vi_diminished_seventh, [9, 0, 3, 6]).
chord_of_c_minor(vii_major, [10, 2, 5]).
chord_of_c_minor(vii_diminished, [11, 2, 5]).
chord_of_c_minor(vii_diminished_seventh, [11, 2, 5, 8]).

root_pitch_of_chord(key(KeyNote, major), RNA, Root) :-
    chord_of_major_key(KeyNote, RNA, [Root|_]).

root_pitch_of_chord(key(KeyNote, minor), RNA, Root) :-
    chord_of_minor_key(KeyNote, RNA, [Root|_]).

root_pitch_of_chord(key(KeyNote, major), RNA, Root) :-
    chord_of_major_key(KeyNote, RNA, [Root|_]).

root_pitch_of_chord(key(KeyNote, minor), RNA, Root) :-
    chord_of_minor_key(KeyNote, RNA, [Root|_]).

root_of_chord_by_rna(Key, RNA, Root) :-
    root_pitch_of_chord(Key, RNA, Pitch),
    note(Pitch, Root).

mode_of_chord(french_sixth, fr_6).
mode_of_chord(german_sixth, ger_6).
mode_of_chord(i_major, maj).
mode_of_chord(i_minor, min).
mode_of_chord(ii_diminished, dim).
mode_of_chord(ii_diminished_seventh, dim_7).
mode_of_chord(ii_minor, min).
mode_of_chord(iii_augmented, aug).
mode_of_chord(iii_major, maj).
mode_of_chord(iii_minor, min).
mode_of_chord(italian_sixth, it_6).
mode_of_chord(iv_major, maj).
mode_of_chord(iv_minor, min).
mode_of_chord(neapolitan_sixth, maj_b).
mode_of_chord(v_dominant_seventh, dom_7).
mode_of_chord(v_major, maj).
mode_of_chord(v_minor, min).
mode_of_chord(vi_diminished, dim).
mode_of_chord(vi_diminished_seventh, dim_7).
mode_of_chord(vi_major, maj).
mode_of_chord(vi_minor, min).
mode_of_chord(vii_diminished, dim).
mode_of_chord(vii_diminished_seventh, dim_7).
mode_of_chord(vii_major, maj).

notes_of_chord(Root, ChordType, Notes) :-
    offsets_of_chord(ChordType, Offsets),
    add_offsets_to_root(Root, Offsets, Notes).

offsets_of_chord(aug, [0, 4, 8]).
offsets_of_chord(it_6, [0, 4, 10]).
offsets_of_chord(ger_6, [0, 4, 7, 10]).
offsets_of_chord(fr_6, [0, 4, 6, 10]).
offsets_of_chord(dim, [0, 3, 6]).
offsets_of_chord(dim_7, [0, 3, 6, 9]).
offsets_of_chord(dom_7, [0, 4, 7, 10]).
offsets_of_chord(maj, [0, 4, 7]).
offsets_of_chord(maj_b, [0, 3, 8]).
offsets_of_chord(min, [0, 3, 7]).

add_offsets_to_root(_, [], []).

add_offsets_to_root(Root, [Offset|Offsets], [Note|Notes]) :-
    note(Position, Root),
    OffsetPosition is (Position + Offset) mod 12,
    note(OffsetPosition, Note),
    add_offsets_to_root(Root, Offsets, Notes).

% vim: ft=prolog
