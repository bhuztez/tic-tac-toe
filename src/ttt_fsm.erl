-module(ttt_fsm).

-behaviour(gen_fsm).

-export([start_link/1, init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

-export(['PING'/2, 'PONG'/2, 'TURN'/2, 'RESULT'/2]).

start_link(Players) ->
    gen_fsm:start_link(?MODULE, [Players], []).


init_state([Noughter, Crosser]) ->
    {array:new([{size, 9},{fixed, true},{default, none}]), Noughter, Crosser}. 



handle_turn(timeout, _StateData) ->
    timeout;
handle_turn({Noughter, [X, 13, 10]}, {Board, Noughter, Crosser}) ->
    none = array:get(X-48, Board),
    gen_tcp:send(Crosser, [X, 13, 10]),
    { array:set(X-48, nought, Board), Noughter, Crosser};
handle_turn({Crosser, [X, 13, 10]}, {Board, Noughter, Crosser}) ->
    none = array:get(X-48, Board),
    gen_tcp:send(Noughter, [X, 13, 10]),
    { array:set(X-48, cross, Board), Noughter, Crosser}.


win(Side, Board) ->
    lists:any(fun (Line) -> Line == {Side, Side, Side} end, 
	      [ { array:get(A, Board), array:get(B, Board), array:get(C, Board) } ||
		  {A,B,C} <- [{0,1,2},{3,4,5},{6,7,8},{0,3,6},{1,4,7},{2,5,8},{0,4,8},{2,4,6}] ]).


result(Noughter, {Board, Noughter, Crosser}) ->
    case win(nought, Board) of
	true ->
	    [{Noughter, "WIN"}, {Crosser, "LOSE"}];
	_ ->
	    Crosser
    end;

result(Crosser, {Board, Noughter, Crosser}) ->
    case win(cross, Board) of
	true ->
	    [{Noughter, "LOSE"}, {Crosser, "WIN"}];
	_ ->
	    Noughter
    end.


handle_result(Player, State = {Board, Noughter, Crosser}) ->
    case lists:all(fun (Grid) -> Grid /= none end , array:to_list(Board)) of
	true ->
	    [{Noughter, "DRAW"}, {Crosser, "DRAW"}];
	_ ->
	    result(Player, State)
    end.


init([Players = [Head | _Tail]]) ->
    [ gen_tcp:send(Player, "READY\r\n") || Player <- Players],
    {ok, 'PING', {Head, init_state(Players)}, 0}.

'PING' (timeout, StateData = {Player, _State}) ->
    gen_tcp:send(Player, "PING 30\r\n"),
    {next_state, 'PONG', StateData, 5000}.

'PONG' ({Player, "PONG\r\n"}, StateData = {Player, _State}) ->
    {next_state, 'TURN', StateData, 30000};

'PONG' ({Player, Data}, StateData) ->
    {stop, {badclient, Player, Data}, StateData};

'PONG' (timeout, StateData = {Player, _State}) ->
    {stop, {timeout, Player}, StateData}.


'TURN' ({Player, Data}, StateData = {Expected, _State}) when Player /= Expected ->
    {stop, {badclient, Player, Data}, StateData};

'TURN' (Event, StateData = {Player, State}) ->
    case catch handle_turn(Event, State) of
        timeout ->
	    {stop, {timeout, Player}, StateData};
	{'EXIT', Reason} ->
	    {Player, Data} = Event,
	    {stop, {badclient, Player, Data, Reason}, StateData};
	NextState ->
	    {next_state, 'RESULT', {Player, NextState}, 0}
    end.


'RESULT' (timeout, StateData = {Player, State}) ->
    case handle_result(Player, State) of
	Results when is_list(Results) ->
	    [ gen_tcp:send(P, Result ++ [13, 10]) || {P, Result} <- Results ],
	    {stop, {result, Results}, StateData};
	NextPlayer ->
	    {next_state, 'PING', {NextPlayer, State}, 0}
    end.


handle_info(_Msg, StateName, StateData) ->
    {noreply, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {noreply, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.


terminate(_Reason, _StateName, _StateData) ->
    gen_server:cast(ttt_room_server, self()).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.



