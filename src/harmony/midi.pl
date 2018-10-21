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

:- module(midi, [pitch_in_range/2, midi_pitch/2, midi_pitches/2]).

:- use_module(library(clpfd)).

pitch_in_range(Voice, Pitch) :-
    vocal_range(Voice, Lower, Upper),
    Pitch #>= Lower,
    Pitch #=< Upper.

% MIDI note values
vocal_range(soprano, 60, 81). % C4 - A5
vocal_range(alto, 55, 74). % G3 - D5
vocal_range(tenor, 48, 69). % C3 - G5
vocal_range(bass, 38, 60). % D2 - C4

midi_pitch(c,    36).
midi_pitch(c_sh, 37).
midi_pitch(d,    38).
midi_pitch(e_fl, 39).
midi_pitch(e,    40).
midi_pitch(f,    41).
midi_pitch(f_sh, 42).
midi_pitch(g,    43).
midi_pitch(g_sh, 44).
midi_pitch(a,    45).
midi_pitch(b_fl, 46).
midi_pitch(b,    47).
midi_pitch(c,    48).
midi_pitch(c_sh, 49).
midi_pitch(d,    50).
midi_pitch(e_fl, 51).
midi_pitch(e,    52).
midi_pitch(f,    53).
midi_pitch(f_sh, 54).
midi_pitch(g,    55).
midi_pitch(g_sh, 56).
midi_pitch(a,    57).
midi_pitch(b_fl, 58).
midi_pitch(b,    59).
midi_pitch(c,    60).
midi_pitch(c_sh, 61).
midi_pitch(d,    62).
midi_pitch(e_fl, 63).
midi_pitch(e,    64).
midi_pitch(f,    65).
midi_pitch(f_sh, 66).
midi_pitch(g,    67).
midi_pitch(g_sh, 68).
midi_pitch(a,    69).
midi_pitch(b_fl, 70).
midi_pitch(b,    71).
midi_pitch(c,    72).
midi_pitch(c_sh, 73).
midi_pitch(d,    74).
midi_pitch(e_fl, 75).
midi_pitch(e,    76).
midi_pitch(f,    77).
midi_pitch(f_sh, 78).
midi_pitch(g,    79).
midi_pitch(g_sh, 80).
midi_pitch(a,    81).
midi_pitch(b_fl, 82).
midi_pitch(b,    83).

midi_pitches([], []).

midi_pitches([Note|Notes], [Pitch|Pitches]) :-
    midi_pitch(Note, Pitch),
    midi_pitches(Notes, Pitches).

% vim: ft=prolog
