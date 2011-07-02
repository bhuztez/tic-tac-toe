-module(gen_turn).

-behaviour(gen_fsm).

-export([start_link/3, start_link/4, behaviour_info/1]).

-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

-export(['PING'/2, 'PONG'/2, 'TURN'/2, 'RESULT'/2]).

behaviour_info(callbacks) ->
    [{init,1}, {handle_turn, 2}, {handle_result, 2}, {code_change, 3}];
behaviour_info(_Other) ->
    undefined.


start_link(Mod, Args, Options) ->
    gen_fsm:start_link(?MODULE, [Mod, Args], Options).

start_link(Name, Mod, Args, Options) ->
    gen_fsm:start_link(Name, ?MODULE, [Mod, Args], Options).


init([Mod, [ [Head | _Tail] = Players]]) ->
    [ gen_tcp:send(Player, "READY\r\n") || Player <- Players],
    {ok, 'PING', {Mod, Head, Mod:init(Players)}, 0}.


'PING' (timeout, StateData = {_Mod, Player, _State}) ->
    gen_tcp:send(Player, "PING 30\r\n"),
    {next_state, 'PONG', StateData, 5000}.

'PONG' ({Player, "PONG\r\n"}, StateData = {_Mod, Player, _State}) ->
    {next_state, 'TURN', StateData, 30000};

'PONG' ({Player, Data}, StateData) ->
    {stop, {badclient, Player, Data}, StateData};

'PONG' (timeout, StateData = {_Mod, Player, _State}) ->
    {stop, {timeout, Player}, StateData}.


'TURN' ({Player, Data}, StateData = {_Mod, Expected, _State}) when Player /= Expected ->
    {stop, {badclient, Player, Data}, StateData};

'TURN' (Event, StateData = {Mod, Player, State}) ->
    case catch Mod:handle_turn(Event, State) of
        timeout ->
	    {stop, {timeout, Player}, StateData};
	{'EXIT', Reason} ->
	    {Player, Data} = Event,
	    {stop, {badclient, Player, Data, Reason}, StateData};
	NextState ->
	    {next_state, 'RESULT', {Mod, Player, NextState}, 0}
    end.


'RESULT' (timeout, StateData = {Mod, Player, State}) ->
    case Mod:handle_result(Player, State) of
	Results when is_list(Results) ->
	    [ gen_tcp:send(P, Result ++ [13, 10]) || {P, Result} <- Results ],
	    {stop, {result, Results}, StateData};
	NextPlayer ->
	    {next_state, 'PING', {Mod, NextPlayer, State}, 0}
    end.



handle_info(_Msg, StateName, StateData) ->
    {noreply, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {noreply, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    gen_server:cast(ttt_room_server, self()).

code_change(OldVsn, StateName, {Mod, Player, State}, Extra) ->
    {ok, NewState} = Mod:code_change(OldVsn, State, Extra),
    {ok, StateName, {Mod, Player, NewState}}.
