note(0, c).
note(1, c_sh).
note(2, d).
note(3, e_fl).
note(4, e).
note(5, f).
note(6, f_sh).
note(7, g).
note(8, g_sh).
note(9, a).
note(10, b_fl).
note(11, b).

chord_of_major(i_major, [0, 4, 7]).
chord_of_major(neapolitan_sixth, [1, 5, 8]).
chord_of_major(ii_minor, [2, 5, 9]).
chord_of_major(iii_minor, [4, 7, 11]).
chord_of_major(iv_minor, [5, 8, 0]).
chord_of_major(iv_major, [5, 9, 0]).
chord_of_major(v_major, [7, 11, 2]).
chord_of_major(v_dominant_seventh, [7, 11, 2, 5]).
chord_of_major(italian_sixth, [8, 0, 6]).
chord_of_major(german_sixth, [8, 0, 3, 6]).
chord_of_major(french_sixth, [8, 0, 2, 6]).
chord_of_major(vi_minor, [9, 0, 4]).
chord_of_major(vii_diminished, [11, 2, 5]).
chord_of_major(vii_diminished_seventh, [11, 2, 5, 8]).

chord_of_minor(i_minor, [0, 3, 7]).
chord_of_minor(neapolitan_sixth, [1, 5, 8]).
chord_of_minor(ii_diminished, [2, 5, 8]).
chord_of_minor(ii_diminished_seventh, [2, 5, 8, 11]).
chord_of_minor(ii_minor, [2, 5, 9]).
chord_of_minor(iii_major, [3, 7, 10]).
chord_of_minor(iii_augmented, [3, 7, 11]).
chord_of_minor(iv_minor, [5, 8, 0]).
chord_of_minor(iv_major, [5, 9, 0]).
chord_of_minor(v_minor, [7, 10, 2]).
chord_of_minor(v_major, [7, 11, 2]).
chord_of_minor(v_dominant_seventh, [7, 11, 2, 5]).
chord_of_minor(italian_sixth, [8, 0, 6]).
chord_of_minor(german_sixth, [8, 0, 3, 6]).
chord_of_minor(french_sixth, [8, 0, 2, 6]).
chord_of_minor(vi_major, [8, 0, 3]).
chord_of_minor(vi_diminished, [9, 0, 3]).
chord_of_minor(vi_diminished_seventh, [9, 0, 3, 6]).
chord_of_minor(vii_major, [10, 2, 5]).
chord_of_minor(vii_diminished, [11, 2, 5]).
chord_of_minor(vii_diminished_seventh, [11, 2, 5, 8]).

pivot_chord(Key1, Key2, pivot(chord(Key1, Name1), chord(Key2, Name2))) :-
    chord_of_key(Key1, Name1, Notes),
    chord_of_key(Key2, Name2, Notes).

chord_of_key(key(Pitch, major), Name, Notes) :- chord_of_major_key(Pitch, Name, Notes).
chord_of_key(key(Pitch, minor), Name, Notes) :- chord_of_minor_key(Pitch, Name, Notes).

chord_of_major_key(Pitch, Name, Notes) :-
    note(Offset, Pitch),
    chord_of_major(Name , OriginalNotes),
    offset_chord(Offset, OriginalNotes, Notes).

chord_of_minor_key(Pitch, Name, Notes) :-
    note(Offset, Pitch),
    chord_of_minor(Name, OriginalNotes),
    offset_chord(Offset, OriginalNotes, Notes).

offset_chord(Offset, OriginalNotes, Notes) :-
    add_offset(Offset, OriginalNotes, OffsetNotes),
    sort(OffsetNotes, Notes).

add_offset(_, [], []).

add_offset(Offset, [X|T], [Y|U]) :-
    Y is (X + Offset) mod 12,
    add_offset(Offset, T, U).

note_names([], []).

note_names([X|T], [Y|U]) :-
    note(X, Y),
    note_names(T, U).

key_chain(Chain) :-
    key_chain([c, g, d, a, e, b, f_sh, c_sh, g_sh, e_fl, b_fl, f], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], Chain).

key_chain([], [], _).

key_chain(AvailableKeys, AvailableIntervals, [key(Key, Mode)|[key(PrevKey, PrevMode)|Chain]]) :-
    member(Key, AvailableKeys),
    note(Offset, Key),
    note(PrevOffset, PrevKey),
    interval(Offset, PrevOffset, Interval),
    member(Interval, AvailableIntervals),
    except(Key, AvailableKeys, RemainingKeys),
    except(Interval, AvailableIntervals, RemainingIntervals),
    random_mode(Mode),
    key_chain(RemainingKeys, RemainingIntervals, [key(PrevKey, PrevMode)|Chain]).

key_chain(AvailableKeys, AvailableIntervals, [key(Key, Mode)]) :-
    member(Key, AvailableKeys),
    except(Key, AvailableKeys, RemainingKeys),
    random_mode(Mode),
    key_chain(RemainingKeys, AvailableIntervals, []).

random_mode(major).
random_mode(minor).

interval(Offset1, Offset2, Interval) :-
    Difference is Offset2 - Offset1,
    positive_interval(Difference, Interval).

positive_interval(Difference, Difference) :-
    abs(Difference, Difference), !.

positive_interval(Difference, Interval) :-
    Interval is Difference + 12.

except(_, [], []).

except(Key, [Key|T], U) :-
    except(Key, T, U), !.

except(Key, [X|T], [X|U]) :-
    except(Key, T, U).

modulating_sequence(Sequence) :-
    key_chain(Chain),
    pivot_chords(Chain, Sequence).

pivot_chords([Key1, Key2|RemainingChain], [Chord|RemainingChords]) :-
    pivot_chord(Key1, Key2, Chord),
    pivot_chords([Key2|RemainingChain], RemainingChords).

pivot_chords([_], []).

padded_sequence(PaddedSequence) :-
    modulating_sequence(Sequence),
    padded_modulating_sequence(Sequence, PaddedSequence).

padded_modulating_sequence([Pivot|RemainingPivots], [Pad, Pivot|RemainingPadded]) :-
    initial_padding(Pivot, Pad),
    trailing_pad([Pivot|RemainingPivots], [Pivot|RemainingPadded]).

trailing_pad([Pivot1, Pivot2|RemainingPivots], [Pivot1, Pad, Pivot2|RemainingPadding]) :-
    intermediate_padding(Pivot1, Pivot2, Pad),
    trailing_pad([Pivot2|RemainingPivots], [Pivot2|RemainingPadding]).

trailing_pad([Pivot], [Pivot, Pad]) :-
    final_padding(Pivot, Pad).

initial_padding(pivot(chord(Key, Chord), _), Pad) :-
    first_pad(Key, Chord, Pad).

intermediate_padding(pivot(_, chord(Key, Chord1)), pivot(chord(Key, Chord2), _), Pad) :-
    inter_pad(Key, Chord1, Chord2, Pad).

final_padding(pivot(_, chord(Key, Chord)), Pad) :-
    final_pad(Key, Chord, Pad).

first_pad(_, i_major, pad([])).

first_pad(key(Key, major), Chord, pad([chord(key(Key, major), i_major)])) :-
    followed_by(i_major, Chord).

first_pad(key(Key, major), Chord, pad([chord(key(Key, major), i_major), chord(Key, major), Chord2])) :-
    followed_by(i_major, Chord2),
    followed_by(Chord2, Chord).

inter_pad(key(_, major), Chord1, Chord2, pad([])) :-
    followed_by(Chord1, Chord2).

inter_pad(key(Key, major), Chord1, Chord2, pad([chord(key(Key, major), IntermediateChord)])) :-
    followed_by(Chord1, IntermediateChord),
    followed_by(IntermediateChord, Chord2).

inter_pad(key(Key, major), Chord1, Chord2, pad([chord(key(Key, major), IntermediateChord1), chord(key(Key, major), IntermediateChord2)])) :-
    followed_by(Chord1, IntermediateChord1),
    followed_by(IntermediateChord1, IntermediateChord2),
    followed_by(IntermediateChord2, Chord2).

final_pad(Key, v_major, pad([chord(Key, i_major)])).

final_pad(Key, Chord, pad([chord(Key, v_major), chord(Key, i_major)])) :-
    followed_by(Chord, v_major).

final_pad(Key, Chord, pad([chord(Key, IntChord), chord(Key, v_major), chord(Key, i_major)])) :-
    followed_by(Chord, IntChord),
    followed_by(IntChord, v_major).

followed_by(i_major, iv_major).
followed_by(i_major, iv_minor).
followed_by(i_major, v_major).
followed_by(i_major, vi_minor).
followed_by(i_major, german_sixth).
followed_by(i_major, italian_sixth).
followed_by(i_major, french_sixth).
followed_by(i_major, vii_diminished).
followed_by(i_major, vii_diminished_seventh).
followed_by(i_major, neapolitan_sixth).

followed_by(i_minor, iv_major).
followed_by(i_minor, iv_minor).
followed_by(i_minor, v_major).
followed_by(i_minor, vi_minor).
followed_by(i_minor, german_sixth).
followed_by(i_minor, italian_sixth).
followed_by(i_minor, french_sixth).
followed_by(i_minor, vii_diminished).
followed_by(i_minor, vii_diminished_seventh).
followed_by(i_minor, neapolitan_sixth).

followed_by(neapolitan_sixth, v_major).

followed_by(ii_minor, v_major).
followed_by(ii_minor, iv_major).
followed_by(ii_minor, iv_minor).
followed_by(ii_minor, vi_minor).

followed_by(ii_diminished, v_major).
followed_by(ii_diminished, iv_major).
followed_by(ii_diminished, iv_minor).
followed_by(ii_diminished, vi_major).

followed_by(iii_minor, vi_minor).
followed_by(iii_minor, iv_major).
followed_by(iii_minor, iv_minor).

followed_by(iii_major, vi_major).
followed_by(iii_major, iv_major).
followed_by(iii_major, iv_minor).

followed_by(iii_augmented, vi_diminished).
followed_by(iii_augmented, iv_major).

followed_by(iv_major, v_major).
followed_by(iv_major, i_major).
followed_by(iv_major, ii_minor).

followed_by(iv_minor, v_major).
followed_by(iv_minor, v_minor).
followed_by(iv_minor, i_major).
followed_by(iv_minor, i_minor).
followed_by(iv_minor, ii_minor).
followed_by(iv_minor, ii_diminished).

followed_by(v_major, i_major).
followed_by(v_major, iv_major).
followed_by(v_major, iv_minor).
followed_by(v_major, neapolitan_sixth).
followed_by(v_major, german_sixth).
followed_by(v_major, italian_sixth).
followed_by(v_major, french_sixth).

followed_by(v_minor, i_minor).
followed_by(v_minor, iv_minor).
followed_by(v_minor, neapolitan_sixth).

followed_by(german_sixth, v_major).

followed_by(italian_sixth, v_major).

followed_by(french_sixth, v_major).

followed_by(vi_minor, ii_minor).
followed_by(vi_minor, v_major).
followed_by(vi_minor, iii_minor).
followed_by(vi_minor, iv_major).

followed_by(vi_major, ii_diminished).
followed_by(vi_major, v_major).
followed_by(vi_major, v_minor).
followed_by(vi_major, iii_major).
followed_by(vi_major, iv_minor).

followed_by(vi_diminished, ii_diminished).
followed_by(vi_diminished, v_major).
followed_by(vi_diminished, v_minor).
followed_by(vi_diminished, iii_major).
followed_by(vi_diminished, iv_major).

followed_by(vii_diminished, i_major).
followed_by(vii_diminished, iii_minor).
followed_by(vii_diminished, vi_minor).

followed_by(vii_diminished_seventh, i_major).
followed_by(vii_diminished_seventh, iii_minor).
followed_by(vii_diminished_seventh, vi_minor).

simplified_sequence(Sequence) :-
    padded_sequence(S),
    simplify_sequence(S, Sequence).

simplify_sequence([], []).

simplify_sequence([pad([])|T], U) :-
    simplify_sequence(T, U), !.

simplify_sequence([H|T], [S|U]) :-
    simplify(H, S),
    simplify_sequence(T, U).

simplify(pad(Chords), SimplifiedChords) :-
    simplify_chords(Chords, SimplifiedChords).

simplify(pivot(Chord, _), SimplifiedChord) :-
    simplify_chord(Chord, SimplifiedChord).

simplify_chords([], []).

simplify_chords([H|T], [S|U]) :-
    simplify_chord(H, S),
    simplify_chords(T, U).

simplify_chord(chord(Key, Name), [Root, Mode]) :-
    offset_of_name(Name, Offset),
    root_of_chord(Key, Offset, Root),
    mode_of_chord(Name, Mode).

offset_of_name(Name, Offset) :-
    chord_of_major(Name, [Offset|_]), !.

offset_of_name(Name, Offset) :-
    chord_of_minor(Name, [Offset|_]).

root_of_chord(key(Note, _), Offset, Root) :-
    note(Pitch, Note),
    Base is (Pitch + Offset) mod 12,
    note(Base, Root).

mode_of_chord(french_sixth, aug_6).
mode_of_chord(german_sixth, aug_6).
mode_of_chord(i_major, maj).
mode_of_chord(i_minor, min).
mode_of_chord(ii_diminished, dim).
mode_of_chord(ii_diminished_seventh, dim_7).
mode_of_chord(ii_minor, min).
mode_of_chord(iii_augmented, aug).
mode_of_chord(iii_major, maj).
mode_of_chord(iii_minor, min).
mode_of_chord(italian_sixth, aug_6).
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

% vim: ft=prolog
