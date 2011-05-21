-module(ttt).
-export([start/0, loop/2, server/2, find_partner/2]).


start() ->
    Server = spawn(?MODULE, server, [dict:new(), none]),
    case gen_tcp:listen(
            9999,
            [list, {packet,line}, {reuseaddr, true}, {active, false}]) of
        {ok, LSocket} ->
            {ok, accept(LSocket, Server)};
        {error, Reason} ->
            {stop, Reason}
    end.


accept(LSocket, Server) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Server ! {login, Socket},
    spawn(?MODULE, loop, [Socket, Server]),
    accept(LSocket, Server).


loop(Socket, Server) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Server ! {data, Socket, Data},
            loop(Socket, Server);
        {error, closed} ->
            Server ! {logout, Socket},
            ok
    end.


server(Users, Waiting) ->
    receive
        {login, User} ->
            case Waiting of
                none ->
                    server(Users, User);
                AnotherUser ->
                    server(dict:append(AnotherUser, User, dict:append(User, AnotherUser, Users)), none)
            end;
        {logout, User} ->
            if User == Waiting ->
                server(Users, none);
            true ->
                case find_partner(User, Users) of
                    error ->
                        server(Users, Waiting);
                    Partner ->
                        gen_tcp:close(Partner),
                        server(dict:erase(User, dict:erase(Partner, Users)), Waiting)
                end
            end;
        {data, User, Data} ->
            Partner = find_partner(User, Users),
            gen_tcp:send(Partner, Data),
            server(Users, Waiting)
    end.


find_partner(User, Users) ->
    case dict:find(User, Users) of
        {ok, [Partner]} ->
            Partner;
        _ ->
            error
    end.

