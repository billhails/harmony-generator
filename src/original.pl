use_module(library(clpfd)).

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

note_names([], []).

note_names([X|T], [Y|U]) :-
    note(X, Y),
    note_names(T, U).

/** key_chain ********************/

key_chain(Chain) :-
    key_chain([c, g, d, a, e, b, f_sh, c_sh, g_sh, e_fl, b_fl, f],
              [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
              Chain).

key_chain([], [], _).

key_chain(AvailableKeys, AvailableIntervals, [key(KeyNote, Mode)|[key(PrevKeyNote, PrevMode)|Chain]]) :-
    member(KeyNote, AvailableKeys),
    note(Offset, KeyNote),
    note(PrevOffset, PrevKeyNote),
    interval(Offset, PrevOffset, Interval),
    member(Interval, AvailableIntervals),
    except(KeyNote, AvailableKeys, RemainingKeys),
    except(Interval, AvailableIntervals, RemainingIntervals),
    random_mode(Mode),
    key_chain(RemainingKeys, RemainingIntervals, [key(PrevKeyNote, PrevMode)|Chain]).

key_chain(AvailableKeys, AvailableIntervals, [key(KeyNote, Mode)]) :-
    member(KeyNote, AvailableKeys),
    except(KeyNote, AvailableKeys, RemainingKeys),
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

/** modulating_sequence ********************/

modulating_sequence(Sequence, UsedNotes) :-
    key_chain(Chain),
    pivot_chords(Chain, Sequence, []),
    extract_notes(Sequence, UsedNotes).

pivot_chords([Key1, Key2|RemainingChain], [Chord|RemainingChords], UsedNotes) :-
    pivot_chord(Key1, Key2, Chord, Notes),
    \+ member(Notes, UsedNotes),
    pivot_chords([Key2|RemainingChain], RemainingChords, [Notes|UsedNotes]).

pivot_chords([_], [], _).

pivot_chord(Key1, Key2, pivot(chord(Key1, RNA1, Notes), chord(Key2, RNA2, Notes)), Notes) :-
    chord_of_key(Key1, RNA1, Notes),
    chord_of_key(Key2, RNA2, Notes).

extract_notes([], []).

extract_notes([pivot(chord(_, _, Notes), _)|RemainingPivots], [Notes|RemainingNotes]) :-
    extract_notes(RemainingPivots, RemainingNotes).

/** padded_sequence *************************/

padded_sequence(PaddedSequence) :-
    modulating_sequence(Sequence, UsedNotes),
    padded_modulating_sequence(Sequence, UsedNotes, PaddedSequence).

padded_modulating_sequence([Pivot|RemainingPivots], UsedNotes, [Pad, Pivot|RemainingPadded]) :-
    initial_padding(Pivot, UsedNotes, Pad, ExtraUsedNotes),
    append(UsedNotes, ExtraUsedNotes, AllUsedNotes),
    trailing_pad([Pivot|RemainingPivots], AllUsedNotes, [Pivot|RemainingPadded]).

initial_padding(pivot(chord(Key, RNA, _), _), UsedNotes, Pad, ExtraUsedNotes) :-
    first_pad(Key, RNA, UsedNotes, Pad, ExtraUsedNotes).

first_pad(_, i_major, _, pad([]), []).

first_pad(key(KeyNote, major),
          RNA,
          UsedNotes,
          pad([chord(key(KeyNote, major), i_major, Notes)]),
          [Notes]) :-
    followed_by(i_major, RNA),
    chord_of_key(key(KeyNote, major), i_major, Notes),
    \+ member(Notes, UsedNotes).

first_pad(key(KeyNote, major),
          RNA,
          UsedNotes,
          pad([chord(key(KeyNote, major), i_major, Notes1),
               chord(key(KeyNote, major), RNA2, Notes2)]),
          [Notes1, Notes2]) :-
    followed_by(i_major, RNA2),
    followed_by(RNA2, RNA),
    chord_of_key(key(KeyNote, major), i_major, Notes1),
    \+ member(Notes1, UsedNotes),
    chord_of_key(key(KeyNote, major), RNA2, Notes2),
    \+ member(Notes2, [Notes1|UsedNotes]).

trailing_pad([Pivot1, Pivot2|RemainingPivots], UsedNotes, [Pivot1, Pad, Pivot2|RemainingPadding]) :-
    intermediate_padding(Pivot1, Pivot2, UsedNotes, Pad, ExtraUsedNotes),
    append(UsedNotes, ExtraUsedNotes, AllUsedNotes),
    trailing_pad([Pivot2|RemainingPivots], AllUsedNotes, [Pivot2|RemainingPadding]).

trailing_pad([Pivot], UsedNotes, [Pivot, Pad]) :-
    final_padding(Pivot, UsedNotes, Pad).

intermediate_padding(pivot(_, chord(Key, RNA1, _)), pivot(chord(Key, RNA2, _), _), UsedNotes, Pad, ExtraUsedNotes) :-
    inter_pad(Key, RNA1, RNA2, UsedNotes, Pad, ExtraUsedNotes).

inter_pad(key(_, major), RNA1, RNA2, _, pad([]), []) :-
    followed_by(RNA1, RNA2).

inter_pad(key(KeyNote, major), RNA1, RNA2, UsedNotes, pad([chord(key(KeyNote, major), IntermediateRna, Notes)]), [Notes]) :-
    followed_by(RNA1, IntermediateRna),
    followed_by(IntermediateRna, RNA2),
    chord_of_key(key(KeyNote, major), IntermediateRna, Notes),
    \+ member(Notes, UsedNotes).

inter_pad(key(KeyNote, major),
          RNA1,
          RNA2,
          UsedNotes,
          pad([chord(key(KeyNote, major), IntermediateRna1, Notes1),
               chord(key(KeyNote, major), IntermediateRna2, Notes2)]),
          [Notes1, Notes2]) :-
    followed_by(RNA1, IntermediateRna1),
    followed_by(IntermediateRna1, IntermediateRna2),
    followed_by(IntermediateRna2, RNA2),
    chord_of_key(key(KeyNote, major), IntermediateRna1, Notes1),
    \+ member(Notes1, UsedNotes),
    chord_of_key(key(KeyNote, major), IntermediateRna2, Notes2),
    \+ member(Notes2, [Notes1|UsedNotes]).

final_padding(pivot(_, chord(Key, RNA, _)), UsedNotes, Pad) :-
    % print_message(debug, UsedNotes),
    final_pad(Key, RNA, UsedNotes, Pad).

final_pad(Key, v_major, UsedNotes, pad([chord(Key, i_major, Notes)])) :-
    chord_of_key(Key, i_major, Notes),
    \+ member(Notes, UsedNotes).

final_pad(Key, RNA, UsedNotes, pad([chord(Key, v_major, Notes1), chord(Key, i_major, Notes2)])) :-
    followed_by(RNA, v_major),
    chord_of_key(Key, v_major, Notes1),
    \+ member(Notes1, UsedNotes),
    chord_of_key(Key, i_major, Notes2),
    \+ member(Notes2, [Notes1|UsedNotes]).

final_pad(Key, RNA, UsedNotes, pad([chord(Key, IntRna, Notes1), chord(Key, v_major, Notes2), chord(Key, i_major, Notes3)])) :-
    followed_by(RNA, IntRna),
    followed_by(IntRna, v_major),
    chord_of_key(Key, IntRna, Notes1),
    \+ member(Notes1, UsedNotes),
    chord_of_key(Key, v_major, Notes2),
    \+ member(Notes2, [Notes1|UsedNotes]),
    chord_of_key(Key, i_major, Notes3),
    \+ member(Notes3, [Notes2, Notes1|UsedNotes]).

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

/** simplified_sequence ********************/

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

simplify_chord(chord(Key, RNA, _), [Root, Mode]) :-
    offset_of_name(RNA, Offset),
    root_of_chord(Key, Offset, Root),
    mode_of_chord(RNA, Mode).

offset_of_name(Name, Offset) :-
    chord_of_c_major(Name, [Offset|_]), !.

offset_of_name(Name, Offset) :-
    chord_of_c_minor(Name, [Offset|_]).

root_of_chord(key(KeyNote, _), Offset, Root) :-
    note(Pitch, KeyNote),
    Base is (Pitch + Offset) mod 12,
    note(Base, Root).

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

/** arranged_sequence ******************/

arranged_sequence(Sequence) :-
    % padded_sequence(S),
    S = [pad([chord(key(c, major), i_major, [0, 4, 7])]),
         pivot(chord(key(c, major), iv_minor, [0, 5, 8]),
               chord(key(c_sh, major), iii_minor, [0, 5, 8])),
         pad([chord(key(c_sh, major), vi_minor, [1, 5, 10])]),
         pivot(chord(key(c_sh, major), v_major, [0, 3, 8]),
               chord(key(e_fl, major), iv_major, [0, 3, 8])),
         pad([]),
         pivot(chord(key(e_fl, major), i_major, [3, 7, 10]),
               chord(key(d, major), neapolitan_sixth, [3, 7, 10])),
         pad([chord(key(d, major), v_major, [1, 4, 9])]),
         pivot(chord(key(d, major), i_major, [2, 6, 9]),
               chord(key(g, major), v_major, [2, 6, 9])),
         pad([chord(key(g, major), i_major, [2, 7, 11])]),
         pivot(chord(key(g, major), vii_diminished_seventh, [0, 3, 6, 9]),
               chord(key(b_fl, major), vii_diminished_seventh, [0, 3, 6, 9])),
         pad([chord(key(b_fl, major), vi_minor, [2, 7, 10])]),
         pivot(chord(key(b_fl, major), ii_minor, [0, 3, 7]),
               chord(key(g_sh, major), iii_minor, [0, 3, 7])),
         pad([]),
         pivot(chord(key(g_sh, major), iv_minor, [1, 4, 8]),
               chord(key(e, major), vi_minor, [1, 4, 8])),
         pad([chord(key(e, major), ii_minor, [1, 6, 9]),
              chord(key(e, major), iv_minor, [0, 4, 9])]),
         pivot(chord(key(e, major), i_major, [4, 8, 11]),
               chord(key(b, major), iv_major, [4, 8, 11])),
         pad([chord(key(b, major), i_major, [3, 6, 11])]),
         pivot(chord(key(b, major), vii_diminished_seventh, [1, 4, 7, 10]),
               chord(key(f, major), vii_diminished_seventh, [1, 4, 7, 10])),
         pad([]),
         pivot(chord(key(f, major), vi_minor, [2, 5, 9]),
               chord(key(a, major), iv_minor, [2, 5, 9])),
         pad([]),
         pivot(chord(key(a, major), ii_minor, [2, 6, 11]),
               chord(key(f_sh, major), iv_minor, [2, 6, 11])),
         pad([chord(key(f_sh, major), v_major, [1, 5, 8]),
              chord(key(f_sh, major), i_major, [1, 6, 10])])],
    filter_notes(S, Chords),
    flatten(Chords, Flattened),
    arrange(Flattened, Sequence).

filter_notes([], []).

filter_notes([pivot(Chord1, Chord2)|RemainingSequence], [pivot(Chord1, Chord2)|RemainingNotes]) :-
    filter_notes(RemainingSequence, RemainingNotes).

filter_notes([pad(Chords)|RemainingSequence], [Notes|RemainingNotes]) :-
    filter_notes(Chords, Notes),
    filter_notes(RemainingSequence, RemainingNotes).

filter_notes([chord(Key, RNA, Notes)|RemainingChords], [chord(Key, RNA, Notes)|RemainingNotes]) :-
    filter_notes(RemainingChords, RemainingNotes).

arrange([Chord|RemainingChords], [SATB|RemainingSATB]) :-
    arrange_first(Chord, SATB),
    arrange_rest([Chord|RemainingChords], [SATB|RemainingSATB]).

extract_chord(chord(Key, RNA, Notes), chord(Key, RNA, Notes)).
extract_chord(pivot(chord(Key, RNA, Notes)), chord(Key, RNA, Notes)).

arrange_first(C, SATB) :-
    extract_chord(C, Chord),
    arrange_first_chord(Chord, SATB).

arrange_first_chord(chord(Key, RNA, _), satb(Key, S, A, T, B)) :-
    root_of_chord_by_rna(Key, RNA, Root),
    mode_of_chord(RNA, Type),
    choose_pitch(bass, Root, Type, [], B),
    choose_pitch(tenor, Root, Type, [B], T),
    choose_pitch(alto, Root, Type, [T, B], A),
    choose_pitch(soprano, Root, Type, [A, T, B], S).

arrange_rest([_], [_]).

arrange_rest([Chord1,Chord2|RemainingChords], [SATB1,SATB2|RemainingSATB]) :-
    arrange_next(Chord1, SATB1, Chord2, SATB2),
    arrange_rest([Chord2|RemainingChords], [SATB2|RemainingSATB]).

arrange_next(C1, SATB1, C2, SATB2) :-
    extract_chord(C1, Chord1),
    extract_chord(C2, Chord2),
    arrange_next_chord(Chord1, SATB1, Chord2, SATB2).

arrange_next_chord(Chord1, SATB1, Chord2, SATB2) :-
    notes_in_common(Chord1, Chord2, CommonNotes),
    arrange_first_chord(Chord2, SATB2).

notes_in_common(chord(_, _, Notes1), chord(_, _, Notes2), CommonNotes) :-
    intersection(Notes1, Notes2, CommonNotes).

root_of_chord_by_rna(Key, RNA, Root) :-
    root_pitch_of_chord(Key, RNA, Pitch),
    note(Pitch, Root).

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

notes_of_chord(Root, ChordType, Notes) :-
    offsets_of_chord(ChordType, Offsets),
    add_offsets_to_root(Root, Offsets, Notes).

add_offsets_to_root(_, [], []).

add_offsets_to_root(Root, [Offset|Offsets], [Note|Notes]) :-
    note(Position, Root),
    OffsetPosition is (Position + Offset) mod 12,
    note(OffsetPosition, Note),
    add_offsets_to_root(Root, Offsets, Notes).

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

pitch_in_range(Voice, Pitch) :-
    vocal_range(Voice, Lower, Upper),
    Pitch #>= Lower,
    Pitch #=< Upper.

% MIDI note values
vocal_range(soprano, 60, 81). % C4 - A5
vocal_range(alto, 55, 74). % G3 - D5
vocal_range(tenor, 48, 69). % C3 - G5
vocal_range(bass, 38, 60). % D2 - C4

midi_pitch(c, 36).
midi_pitch(c_sh, 37).
midi_pitch(d, 38).
midi_pitch(e_fl, 39).
midi_pitch(e, 40).
midi_pitch(f, 41).
midi_pitch(f_sh, 42).
midi_pitch(g, 43).
midi_pitch(g_sh, 44).
midi_pitch(a, 45).
midi_pitch(b_fl, 46).
midi_pitch(b, 47).
midi_pitch(c, 48).
midi_pitch(c_sh, 49).
midi_pitch(d, 50).
midi_pitch(e_fl, 51).
midi_pitch(e, 52).
midi_pitch(f, 53).
midi_pitch(f_sh, 54).
midi_pitch(g, 55).
midi_pitch(g_sh, 56).
midi_pitch(a, 57).
midi_pitch(b_fl, 58).
midi_pitch(b, 59).
midi_pitch(c, 60).
midi_pitch(c_sh, 61).
midi_pitch(d, 62).
midi_pitch(e_fl, 63).
midi_pitch(e, 64).
midi_pitch(f, 65).
midi_pitch(f_sh, 66).
midi_pitch(g, 67).
midi_pitch(g_sh, 68).
midi_pitch(a, 69).
midi_pitch(b_fl, 70).
midi_pitch(b, 71).
midi_pitch(c, 72).
midi_pitch(c_sh, 73).
midi_pitch(d, 74).
midi_pitch(e_fl, 75).
midi_pitch(e, 76).
midi_pitch(f, 77).
midi_pitch(f_sh, 78).
midi_pitch(g, 79).
midi_pitch(g_sh, 80).
midi_pitch(a, 81).
midi_pitch(b_fl, 82).
midi_pitch(b, 83).

midi_pitches([], []).

midi_pitches([Note|Notes], [Pitch|Pitches]) :-
    midi_pitch(Note, Pitch),
    midi_pitches(Notes, Pitches).

% vim: ft=prolog
