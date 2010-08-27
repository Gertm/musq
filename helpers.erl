%%% @author Gert  Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert  Meulyzer
%%% @doc
%%% Helper functions module. For random functions not fitting anywhere else.
%%% @end
%%% Created : 11 Aug 2010 by Gert  Meulyzer <@G3rtm on Twitter>

-module(helpers).

-compile(export_all).

clean_room_name(RoomName) ->
    re:replace(RoomName," ","_",[{return,list}]).

clean_tcp_input(TcpInput) ->
    Len = erlang:round(bit_size(TcpInput)/8),
    {Out,_} = split_binary(TcpInput,Len-2),
    binary_to_list(Out).

recv_string(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    {tcp, Socket, clean_tcp_input(Data)};
	{tcp_closed, Socket} ->
	    io:format("~p Client Disconnected.~n", [erlang:localtime()]),
	    {tcp_closed, Socket};
	Other -> Other
    end.
