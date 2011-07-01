-module(ttt_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    { ok, 
      { {one_for_one, 0, 1},
	[ 
	  { ttt_room_server,
	    { ttt_room_server, start_link, [] },
	    temporary,
	    brutal_kill,
	    worker,
	    [ ttt_room_server ]
	  },

	  { ttt_server,
	    { ttt_server, start_link, [] },
	    temporary,
	    brutal_kill,
	    worker,
	    [ ttt_server ]
	  }

	]
      }
    }.
