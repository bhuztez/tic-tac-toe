-module(ttt_server).

-behavior(gen_server).

-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2, accept/1, loop/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_tcp:listen(9999, [list, {packet,line}, {reuseaddr, true}, {active, false}]),
    spawn_link(fun () ->  accept(Socket) end),
    {ok, Socket}.


accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(?MODULE, {accept, Socket}),
    accept(LSocket).


loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_server:cast(?MODULE, {recv, Socket, Data}),
            loop(Socket);
        {error, closed} ->
            gen_server:cast(?MODULE, {closed, Socket}),
	    ok
    end.


handle_cast({accept, Socket}, State) ->
    spawn_link(fun () ->  loop(Socket) end),
    gen_server:call(ttt_rooms, {login, Socket}),
    {noreply, State};

handle_cast({recv, Socket, Data}, State) ->
    gen_server:cast(ttt_rooms, {data, Socket, Data}),
    {noreply,  State};

handle_cast({closed, Socket}, State) ->
    gen_server:call(ttt_rooms, {logout, Socket}),
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


