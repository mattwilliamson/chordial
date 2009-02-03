%% This is the application resource file (.app file) for the chordial,
%% application.
{application, chordial, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [chordial_app,
              chordial_sup,
              gen_chord]},
   {registered,[chordial_sup]},
   {applications, [kernel, stdlib]},
   {mod, {chordial_app,[]}},
   {start_phases, []}]}.

