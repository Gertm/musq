%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(wshandle).
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
    receive
		{ok, WebSocket} ->
			yaws_api:websocket_setopts(WebSocket, [{active, true}]),			
			echo_server(WebSocket);
		_ -> error_logger:info_msg("Didn't get websocket stuff.. strange!")
    end.

echo_server(WebSocket) ->
    receive
		{tcp, WebSocket, DataFrame} ->
			Data = yaws_api:websocket_unframe_data(DataFrame),
			case Data of
				[<<"Function">>] ->
					yaws_api:websocket_send(WebSocket,"Received function!~n"),
					echo_server(WebSocket);
				_ ->
					io:format("Got ~p~n",[Data]),
					yaws_api:websocket_send(WebSocket,"Not a function!~n"),
					echo_server(WebSocket)
			end;
		{tcp_closed, WebSocket} ->
			io:format("Websocket closed. Terminating echo_server...~n");
		{reply, _From, Reply} ->
			reply(WebSocket,Reply);
		Any ->
			io:format("echo_server received msg:~p~n", [Any]),
			echo_server(WebSocket)
    end.

get_func_and_params(BinData) ->
	%% {"Function":"login","Params":{"Username":"Gert","Password":"g"}}.
	[{<<"Function">>,Func},{<<"Params">>,{struct,Params}}] = mochijson2:decode(BinData),
    {Func,Params}.

reply(WebSocket, Reply) ->
	R = mochijson:encode(Reply),
	io:format("Sending: ~s~n",[R]),
	yaws_api:websocket_send(WebSocket, R).
