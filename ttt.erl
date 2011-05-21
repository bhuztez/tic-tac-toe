-module(ttt).
-export([start/0, loop/1]).


start() ->
    case gen_tcp:listen(
            9999,
            [list, {packet,line}, {reuseaddr, true}, {active, false}]) of
        {ok, LSocket} ->
            {ok, accept(LSocket)};
        {error, Reason} ->
            {stop, Reason}
    end.


accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(?MODULE, loop, [Socket]),
    accept(LSocket).


loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.

