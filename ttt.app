{application, ttt,
 [{description, "tic-tac-toe server"},
  {vsn, "0.0"},
  {modules, [ttt_app]},
  {registered, [ttt_app]},
  {applications, [kernel, stdlib]},
  {mod, {ttt_app, []}}
 ]}.

