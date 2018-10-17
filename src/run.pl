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

:- prolog_load_context(directory, Cwd),
   atomic_concat(Cwd, '/harmony', Dir),
   asserta(user:file_search_path(harmony, Dir)).

:- set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).

:- use_module(library(clpfd)).
:- use_module(harmony(key_chain)).
:- use_module(harmony(pivot_chords)).
:- use_module(harmony(padding)).

% vim: ft=prolog
