:- module(chords, [chord_of_key/3]).

:- use_module(harmony(note)).

chord_of_key(key(KeyNote, major), RNA, Notes) :-
    chord_of_major_key(KeyNote, RNA, OffsetNotes),
    sort(OffsetNotes, Notes).

chord_of_key(key(KeyNote, minor), RNA, Notes) :-
    chord_of_minor_key(KeyNote, RNA, OffsetNotes),
    sort(OffsetNotes, Notes).

chord_of_major_key(KeyNote, RNA, Notes) :-
    note:note(Offset, KeyNote),
    chord_of_c_major(RNA , OriginalNotes),
    offset_chord(Offset, OriginalNotes, Notes).

chord_of_minor_key(KeyNote, RNA, Notes) :-
    note:note(Offset, KeyNote),
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

% vim: ft=prolog
