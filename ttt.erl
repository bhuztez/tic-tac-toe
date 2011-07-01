%%% erlc ttt.erl
%%% erl -s ttt -noshell

-module(ttt).
-export([start/0, server/2, make_room/2, loop/2]).


%%%
%%% ENTRY POINT
%%%

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

%%% 
%%% SOCKET
%%%

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


%%%
%%% ROOM LIST
%%%

server(Users, Waiting) ->
    receive
        {login, User} ->
	    case Waiting of
                none ->
		    server(Users, User);
	        AnotherUser ->
    		    gen_tcp:send(AnotherUser, "READY\r\n"),
		    gen_tcp:send(User, "READY\r\n"),
		    Room = spawn(?MODULE, make_room, [[AnotherUser, User], fun start_ttt/1]),
		    server(dict:append(AnotherUser, Room, dict:append(User, Room, Users)), none)
	    end;

	{logout, User} ->
	    if User == Waiting ->
	        server(Users, none);
            true ->
	        Room = find_room(User, Users),
		Room ! {quit, User},
		server(dict:erase(User, Users), Waiting)
            end;

	{data, User, Data} ->
            Room = find_room(User, Users),
            Room ! {data, User, Data},
	    server(Users, Waiting)
    end.



%%%
%%% ROOM LOGIC
%%%


make_room(Users, Init) ->
    State = Init(Users),
    ping(State, Users).


ping(State, Users) ->
    [ Head | _ ] = Users,

    gen_tcp:send(Head, "PING 30\r\n"),

    receive
        {quit, User} ->
	    disconnect([ U || U <- Users, U /= User ]);

	{data, Head, "PONG\r\n"} ->
            handle(State, Users)

	after 5000 ->
	    disconnect(Users)

    end.


handle(State, Users) ->
    [ Head | Tail ] = Users,

    receive
        {quit, User} ->
	    disconnect([ U || U <- Users, U /= User ]);

	{data, Head, [X,13,10]} ->
            case ttt(State, Head, X) of
                {finish, Result, Winner} ->
                    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, [Result, 13, 10]) end, Tail),
                    gen_tcp:send(Winner, "WIN\r\n"),
                    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, "LOSE\r\n") end,
		                  [ U || U <- Users, U /= Winner]),
	            disconnect(Users);

                {finish, Result} ->
                    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, [Result, 13, 10]) end, Tail),
                    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, "DRAW\r\n") end, Users),
	            disconnect(Users);

                {NextState, Result} ->
                    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, [Result, 13, 10]) end, Tail),
                    ping(NextState, Tail ++ [Head])
            end

	after 30000 ->
	    disconnect(Users)

    end.




%%%
%%% GAME LOGIC
%%%

start_ttt([Noughter, Crosser]) ->
    {array:new([{size, 9},{fixed, true},{default, none}]), Noughter, Crosser}.

win(State, Mark) ->
    Lines = [
        { array:get(0, State), array:get(1, State), array:get(2, State) },
        { array:get(3, State), array:get(4, State), array:get(5, State) },
        { array:get(6, State), array:get(7, State), array:get(8, State) },
        { array:get(0, State), array:get(3, State), array:get(6, State) },
        { array:get(1, State), array:get(4, State), array:get(7, State) },
        { array:get(2, State), array:get(5, State), array:get(8, State) },
        { array:get(0, State), array:get(4, State), array:get(8, State) },
        { array:get(2, State), array:get(4, State), array:get(6, State) } ],

    lists:any(fun (Line) -> Line == {Mark, Mark, Mark} end, Lines).

full(State) ->
    lists:all(fun (Grid) -> Grid /= none end , array:to_list(State)).


ttt({State, Noughter, Crosser}, User, X) ->
    case User of 
        Noughter ->
            Mark = nought;
        Crosser ->
            Mark = cross
    end,

    NextState = array:set(X-48, Mark, State),


    case win(NextState, Mark) of
        true ->
            {finish, X, User};
        _ ->
            case full(NextState) of
                true ->
                    {finish, X};
                _ ->
                    {{NextState, Noughter, Crosser}, X}
            end
    end.



%%%
%%% UTILITIES
%%% 

find_room(User, Users) ->
    case dict:find(User, Users) of
        {ok, [Room]} ->
            Room;
        _ ->
            error
    end.


finish([]) ->
    ok;
finish(Users) ->
    receive
        {quit, User} ->
	    finish([ U || U <- Users, U/=User])
    end.


disconnect(Users) ->
    lists:foreach(fun(Socket) -> gen_tcp:close(Socket) end, Users),
    finish(Users).

