%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(wshandle).
-include_lib("yaws/include/yaws_api.hrl").
-include("musq.hrl").
-compile(export_all).

out(A) ->
    case get_upgrade_header(A#arg.headers) of 
		undefined ->
			{html,"MUSQ server. Please connect through the websocket API."};
		"WebSocket" ->
			WebSocketOwner = spawn(fun() -> websocket_owner() end),
			{websocket, WebSocketOwner, passive}
    end.

get_upgrade_header(#headers{other=L}) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            "upgrade" ->
                                V;
                            _ ->
                                undefined
                        end;
                   (_, Acc) ->
                        Acc
                end, undefined, L).					  


websocket_owner() ->
	NewPlayerPid = gen_server:call(world, {spawn_player, self()}),
	?InfoMsg("New Player spawned with pid ~p~n",[NewPlayerPid]),
    receive
		{ok, WebSocket} ->
			yaws_api:websocket_setopts(WebSocket, [{active, true}]),
			echo_server(WebSocket,NewPlayerPid);
		_ -> error_logger:info_msg("Didn't get websocket stuff.. strange!")
    end.

echo_server(WebSocket,PlayerPid) ->
    receive
		{tcp, WebSocket, DataFrame} ->
			?InfoMsg("Dataframe: ~s~n",[DataFrame]),
			Data = yaws_api:websocket_unframe_data(DataFrame),
			{Fn,Params} = get_func_and_params(Data),
			PlayerPid ! {list_to_atom(Fn),self(),Params},
			?InfoMsg("Got request: ~p~n~p~n",[Fn,Params]),
			echo_server(WebSocket, PlayerPid);
		{tcp_closed, WebSocket} ->
			io:format("Websocket closed. Terminating echo_server...~n");
		{reply, PlayerPid, Reply} ->
			reply(WebSocket,Reply),
			echo_server(WebSocket,PlayerPid);
		Any ->
			?InfoMsg("DEBUG: got unknown: ~p~n",[Any]),
			echo_server(WebSocket,PlayerPid)
    end.

get_func_and_params(BinData) ->
	{struct,[{"Function",Func},{"Params",{struct,Params}}]} = mochijson:decode(BinData),
	io:format("Function: ~s~nParams: <~p>~n",[Func,Params]),
    {Func,Params}.

reply(WebSocket, Reply) ->
	R = mochijson:encode(Reply),
	?InfoMsg("Sending back to the client: ~p~n",[R]),
	yaws_api:websocket_send(WebSocket, R).

test_get_func_and_params() ->
	A = <<"{\"Function\":\"login\",\"Params\":{\"Username\":\"Gert\",\"Password\":\"g\"}}">>,
	get_func_and_params(A).
