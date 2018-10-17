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
