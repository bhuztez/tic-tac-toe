-module(ttt_server).

-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2, accept/1, loop/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, Socket} = gen_tcp:listen(9999, [list, {packet,line}, {reuseaddr, true}, {active, false}]),
    spawn_link(fun () ->  accept(Socket) end),
    {ok, Socket}.


accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(?MODULE, Socket),
    accept(LSocket).


loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    gen_server:cast(?MODULE, {Socket, Data}),
	    loop(Socket);
	{error, closed} ->
	    ok
    end.


handle_cast({Socket, Data}, State) ->
    gen_server:cast(ttt_room_server, {Socket, Data}),
    {noreply,  State};

handle_cast(Socket, State) ->
    spawn(fun () ->  loop(Socket) end),
    gen_server:call(ttt_room_server, Socket),
    {noreply, State}.



handle_call(_Request, _From, State) ->
    {noreply,  State}.


handle_info(Msg, State) ->
    io:format("~p~n", [Msg]),
    {noreply, State}.


terminate(_Reason, LSocket) ->
    gen_tcp:close(LSocket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


