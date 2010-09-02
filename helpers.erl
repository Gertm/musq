%%% @author Gert  Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert  Meulyzer
%%% @doc
%%% Helper functions module. For random functions not fitting anywhere else.
%%% @end
%%% Created : 11 Aug 2010 by Gert  Meulyzer <@G3rtm on Twitter>

-module(helpers).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

clean_room_name(RoomName) ->
    re:replace(RoomName," ","_",[{return, list}]).

clean_room_name_test_() ->
    [?_assert(clean_room_name("test 1") =:= "test_1")].

recv_string(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    case binary_to_list(Data) of
		[X|_] when X > 127 -> recv_string(Socket);
		S ->
		    {R, _} = lists:split(length(S) - 2, S),
		    {tcp, Socket, R}
	    end;
	{tcp_closed, Socket} ->
	    io:format("~p Client Disconnected.~n", [erlang:localtime()]),
	    {tcp_closed, Socket};
	Other -> Other
    end.
