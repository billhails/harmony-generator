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
