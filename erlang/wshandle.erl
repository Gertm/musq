%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(wshandle).
-include("/usr/lib/erlang/lib/yaws/include/yaws_api.hrl").
-compile(export_all).

out(A) ->
    io:format("~p~n",[A]),
    case get_upgrade_header(A#arg.headers) of 
		undefined ->
			Headers = A#arg.headers,
			{html,io_lib:format("You say that you're running ~p",[Headers#headers.user_agent])};
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
			%% This is how we read messages (plural!!) from websockets on passive mode
			case yaws_api:websocket_receive(WebSocket) of
				{error,closed} ->
					io:format("The websocket got disconnected right from the start. "
							  "This wasn't supposed to happen!!~n");
				{ok, Messages} ->
					case Messages of
						[<<"MUSQ">>] ->
							yaws_api:websocket_setopts(WebSocket, [{active, true}]),
							echo_server(WebSocket);
						Other ->
							io:format("websocket_owner got: ~p. NOT MUSQ API!~n", [Other]),
							echo_server(WebSocket)
					end
			end;
		_ -> ok
    end.

echo_server(WebSocket) ->
    receive
		{tcp, WebSocket, DataFrame} ->
			Data = yaws_api:websocket_unframe_data(DataFrame),
			{struct,[JSON]} = mochijson2:decode(Data),
			io:format("~s~n",[JSON]),
			{First,Second} = JSON,
			case First of
				[<<"Function">>] ->
					yaws_api:websocket_send("Received function!~n"),
					echo_server(WebSocket);
				_ ->
					yaws_api:websocket_send("Not a function!~n"),
					echo_server(WebSocket)
			end;
		{tcp_closed, WebSocket} ->
			io:format("Websocket closed. Terminating echo_server...~n");
		Any ->
			io:format("echo_server received msg:~p~n", [Any]),
			echo_server(WebSocket)
    end.


