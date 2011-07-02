-module(ttt_turn).

-behaviour(gen_turn).

-export([start_link/1, init/1, handle_turn/2, handle_result/2, code_change/3]).

start_link(Players) ->
    gen_turn:start_link(?MODULE, [Players], []).


init([Noughter, Crosser]) ->
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

draw(Board) ->
    lists:all(fun (Grid) -> Grid /= none end , array:to_list(Board)).


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
    case draw(Board) of
	true ->
	    [{Noughter, "DRAW"}, {Crosser, "DRAW"}];
	_ ->
	    result(Player, State)
    end.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



