:- module(padding, [padding_chords/2]).

:- use_module(harmony(sequences)).
:- use_module(harmony(chords)).

padding_chords([Pivot|RemainingPivots], [Pad, Pivot|RemainingPadded]) :-
    initial_padding(Pivot, Pad),
    trailing_pad([Pivot|RemainingPivots], [Pivot|RemainingPadded]).

initial_padding(pivot(chord(Key, RNA, _), _), Pad) :-
    first_pad(Key, RNA, Pad).

first_pad(_, i_major, pad([])).

first_pad(key(KeyNote, Mode),
          RNA,
          pad([chord(key(KeyNote, Mode), Tonic, Notes)])) :-
    tonic(Mode, Tonic),
    followed_by(Tonic, RNA),
    chord_of_key(key(KeyNote, Mode), Tonic, Notes).

first_pad(key(KeyNote, Mode),
          RNA,
          pad([chord(key(KeyNote, Mode), Tonic, Notes1),
               chord(key(KeyNote, Mode), RNA2, Notes2)])) :-
    tonic(Mode, Tonic),
    followed_by(Tonic, RNA2),
    followed_by(RNA2, RNA),
    chord_of_key(key(KeyNote, Mode), Tonic, Notes1),
    chord_of_key(key(KeyNote, Mode), RNA2, Notes2).

tonic(major, i_major).
tonic(minor, i_minor).

trailing_pad([Pivot1, Pivot2|RemainingPivots], [Pivot1, Pad, Pivot2|RemainingPadding]) :-
    intermediate_padding(Pivot1, Pivot2, Pad),
    trailing_pad([Pivot2|RemainingPivots], [Pivot2|RemainingPadding]).

trailing_pad([Pivot], [Pivot, Pad]) :-
    final_padding(Pivot, Pad).

intermediate_padding(pivot(_, chord(Key, RNA1, _)), pivot(chord(Key, RNA2, _), _), Pad) :-
    inter_pad(Key, RNA1, RNA2, Pad).

inter_pad(key(_, _), RNA1, RNA2, pad([])) :-
    followed_by(RNA1, RNA2).

inter_pad(key(KeyNote, Mode), RNA1, RNA2, pad([chord(key(KeyNote, Mode), IntermediateRna, Notes)])) :-
    % print_message(debug(padding), [RNA1, RNA2]),
    followed_by(RNA1, IntermediateRna),
    % print_message(debug(padding), [RNA1, IntermediateRna, RNA2]),
    followed_by(IntermediateRna, RNA2),
    chord_of_key(key(KeyNote, Mode), IntermediateRna, Notes).

inter_pad(key(KeyNote, Mode),
          RNA1,
          RNA2,
          pad([chord(key(KeyNote, Mode), IntermediateRna1, Notes1),
               chord(key(KeyNote, Mode), IntermediateRna2, Notes2)]),
          [Notes1, Notes2]) :-
    followed_by(RNA1, IntermediateRna1),
    followed_by(IntermediateRna1, IntermediateRna2),
    followed_by(IntermediateRna2, RNA2),
    chord_of_key(key(KeyNote, Mode), IntermediateRna1, Notes1),
    chord_of_key(key(KeyNote, Mode), IntermediateRna2, Notes2).

final_padding(pivot(_, chord(Key, RNA, _)), Pad) :-
    final_pad(Key, RNA, Pad).

final_pad(Key, v_major, pad([chord(Key, i_major, Notes)])) :-
    chord_of_key(Key, i_major, Notes).

final_pad(Key, RNA, pad([chord(Key, v_major, Notes1), chord(Key, i_major, Notes2)])) :-
    followed_by(RNA, v_major),
    chord_of_key(Key, v_major, Notes1),
    chord_of_key(Key, i_major, Notes2).

final_pad(Key, RNA, pad([chord(Key, IntRna, Notes1), chord(Key, v_major, Notes2), chord(Key, i_major, Notes3)])) :-
    followed_by(RNA, IntRna),
    followed_by(IntRna, v_major),
    chord_of_key(Key, IntRna, Notes1),
    chord_of_key(Key, v_major, Notes2),
    chord_of_key(Key, i_major, Notes3).


% vim: ft=prolog
