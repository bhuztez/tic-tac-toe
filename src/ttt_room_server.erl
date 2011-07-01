-module(ttt_room_server).

-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, {none, dict:new(), 1}}.

handle_call(Client, _From, {none, Clients, NextId}) when is_port(Client) ->
    {reply, ok, {Client, Clients, NextId}};

handle_call(Client, _From, {Waiting, Clients, NextId}) when is_port(Client) ->
    {ok, Room} = supervisor:start_child(ttt_sup,
	  { NextId,
	    { ttt_fsm, start_link, [[Waiting, Client]] },
	    temporary,
	    brutal_kill,
	    worker,
	    [ ttt_fsm ]
	  }),
    NewClients = dict:append(Client, Room, dict:append(Waiting, Room, Clients)),
    {reply, ok, {none, NewClients, NextId+1}}.

handle_cast({Client, Data}, State = {_Waiting, Clients, _NextId}) when is_port(Client) ->
    [Room] = dict:fetch(Client, Clients),
    gen_fsm:send_event(Room, {Client, Data}),
    {noreply, State};
handle_cast(Room, {Waiting, Clients, NextId}) when is_pid(Room) ->
    [ gen_tcp:close(Client) || Client <- dict:fetch_keys(dict:filter(fun (_Key, Value) -> Value =/= Room end, Clients)) ],
    NewClients = dict:filter(fun (_Key, Value) -> Value =:= Room end, Clients),
    {noreply, {Waiting, NewClients, NextId}}.


handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






