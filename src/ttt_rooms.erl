-module(ttt_rooms).

-behavior(gen_server).

-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, {none, dict:new()}}.

handle_call({login, Socket}, _From, {none, Users}) ->
    {reply, ok, {Socket, Users}};

handle_call({login, Socket}, _From, {Waiting, Users}) ->
    NewUsers = dict:append(Socket, Waiting, dict:append(Waiting, Socket, Users)),
    {reply, ok, {none, NewUsers}};

handle_call({logout, Socket}, _From, {Waiting, Users}) ->
    case find_partner(Socket, Users) of
        error ->
	    {reply, ok, {Waiting, Users}};
	Partner ->
	    NewUsers = dict:erase(Socket, dict:erase(Partner, Users)),
	    {reply, ok, {Waiting, NewUsers}}
    end.

handle_cast({data, Socket, Data}, State = {_Waiting, Users}) ->
    case find_partner(Socket, Users) of
        error ->
	    {noreply, State};
	Partner ->
	    gen_tcp:send(Partner, Data),
	    {noreply, State}
    end.


handle_info(Msg, State) ->
    io:format("~p~n", [Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


find_partner(User, Users) ->
    case dict:find(User, Users) of
        {ok, [Partner]} ->
            Partner;
        _ ->
            error
    end.




