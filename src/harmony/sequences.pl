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

:- module(sequences, [followed_by/2]).

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

followed_by(neapolitan_sixth, v_dominant_seventh).
followed_by(neapolitan_sixth, v_major).

followed_by(ii_minor, v_dominant_seventh).
followed_by(ii_minor, v_major).
followed_by(ii_minor, iv_major).
followed_by(ii_minor, iv_minor).
followed_by(ii_minor, vi_minor).

followed_by(ii_diminished, v_dominant_seventh).
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
followed_by(iv_minor, ii_diminished_seventh).

followed_by(v_major, i_major).
followed_by(v_major, iv_major).
followed_by(v_major, iv_minor).
followed_by(v_major, neapolitan_sixth).
followed_by(v_major, german_sixth).
followed_by(v_major, italian_sixth).
followed_by(v_major, french_sixth).

followed_by(v_dominant_seventh, i_minor).
followed_by(v_dominant_seventh, i_major).
followed_by(v_dominant_seventh, iv_major).
followed_by(v_dominant_seventh, iv_minor).
followed_by(v_dominant_seventh, vi_minor).
followed_by(v_dominant_seventh, neapolitan_sixth).
followed_by(v_dominant_seventh, german_sixth).
followed_by(v_dominant_seventh, italian_sixth).
followed_by(v_dominant_seventh, french_sixth).

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
followed_by(vi_major, ii_diminished_seventh).
followed_by(vi_major, v_major).
followed_by(vi_major, v_minor).
followed_by(vi_major, iii_major).
followed_by(vi_major, iv_minor).

followed_by(vi_diminished, ii_diminished).
followed_by(vi_diminished, ii_diminished_seventh).
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

% vim: ft=prolog
