%% This is the application resource file (.app file) for the chordial,
%% application.
{application, chordial, 
  [{description, "Erlang implementation of the Chord distributed hash algorithm"},
   {vsn, "0.1.0"},
   {modules, [chordial_app,
              chordial_sup,
              gen_chord,
              chord_lib,
			  chord_monitor,
              sha1,
              mochihex]},
   {registered, [chordial_sup, chord_monitor]},
   {applications, [kernel, stdlib]},
   {mod, {chordial_app, []}},
   {start_phases, []}]}.

