%%% @author  <Randy Voet <crimson13>>
%%% @copyright (C) 2010, 
%%% @doc
%%%
%%% @end
%%% Created :  3 Sep 2010 by  <Randy Voet <crimson13>>

-module(playercontroller).

%% API
-export([handle/1]).

%% internal functions
-export([loop/2, parse_command/2, send_paragraphs/3, send_area_map_and_description/3]).

-include("telnetcolors.hrl").
-include("records.hrl").
-define(WRAPCOLS, 70).

%%%===================================================================
%%% API
%%%===================================================================

handle(Socket) ->
    motd:motd(Socket),
    gen_tcp:send(Socket, "Please enter your username: "),
    case helpers:recv_string(Socket) of
	{tcp, Socket, Username} ->
	    io:format("User ~s logging in.~n", [Username]),
	    gen_tcp:send(Socket, "Password: "),
	    case helpers:recv_string(Socket) of
		{tcp, Socket, "quit"} ->
		    {'EXIT', "got quit command"};
		{tcp, Socket, Password} ->
		    io:format("Password ~s received.~n", [Password]),
		    case player:start_link([Username, self()]) of
			{ok, PlayerPid} ->
			    %% temporary
			    ?send("Password accepted! Sending you to your starting location."),
			    player:enter_start_room(PlayerPid, room_source1),
			    loop(Socket, PlayerPid);
			_ -> {'EXIT', "problem starting player gen_server"}
		    end;
		_ -> {'EXIT', "connection lost"}
	    end;
	_ -> {'EXIT', "connection lost"}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(Socket, Pid) ->
    gen_tcp:send(Socket, ?PROMPT),
    case helpers:recv_string(Socket) of
	{tcp, Socket, Cmd} ->
	    case parse_command(Cmd, Pid) of
		ok ->
		    loop(Socket, Pid);
		{looked, Desc, AreaMap} ->
		    send_area_map_and_description(Socket, AreaMap, Desc),
		    loop(Socket, Pid);
		{warning, Message} ->
		    ?send(?yellow(Message)),
		    loop(Socket, Pid);
		{error, Message} ->
		    ?send(?red(Message)),
		    loop(Socket, Pid);
		{unknown, _Command} ->
		    ?send(?PROMPT++?red("Sorry, that didn't make any sense.")),
		    loop(Socket, Pid);
		{close, Response} ->
		    ?send(Response);
		Other ->
		    io:format("tcp_receive_loop/parse_command: ~p~n", [Other])
	    end;
	{tcp_closed, Socket} ->
	    %% question is now, what do we do when the player disconnects in mid-combat?
	    %% perhaps we should answer that when we've actually implemented combat ;-)
	    player:save(Pid);
	{notification, Message} ->
	    send_paragraphs(Socket, [Message], ?F_WHITE),
	    loop(Socket, Pid);
	Other ->
	    io:format("tcp_receive_loop: ~p~n", [Other])
    end.

parse_command(Command, Pid) ->
    case Command of
	"quit" -> {close, "Bye"};
	"n" -> player:go("north", Pid);
	"s" -> player:go("south", Pid);
	"w" -> player:go("west", Pid);
	"e" -> player:go("east", Pid);
	"nw" -> player:go("northwest", Pid);
	"ne" -> player:go("northeast", Pid);
	"sw" -> player:go("southwest", Pid);
	"se" -> player:go("southeast", Pid);
	"up" -> player:go("up", Pid);
	"down" -> player:go("down", Pid); %% ugh, there needs to be a better way of doing this.
	"l" -> player:look(Pid);
	"look" -> player:look(Pid);
	_ -> {unknown, Command}
    end.

send_paragraphs(Socket, Paragraphs, Color) ->
    SendLine = fun(Line) -> ?send(?with_color(Line, Color)) end,
    SendParagraph = fun(Paragraph) ->
			    Lines = helpers:wrap(Paragraph, ?WRAPCOLS),
			    lists:foreach(SendLine, Lines)
		    end,
    lists:foreach(SendParagraph, Paragraphs).

send_area_map_and_description(Socket, Map, [Title|Desc]) ->
    MapSize = ?AREAMAPRADIUS * 2 + 1,
    WrapColumns = ?WRAPCOLS - MapSize - 2,
    WrappedDesc = lists:append([helpers:wrap(DescLine, WrapColumns) || DescLine <- Desc]),
    CombinedHeight = erlang:max(1 + length(WrappedDesc), MapSize),
    AreaMapLines = helpers:render_area_map(Map, MapSize, CombinedHeight),
    SeparatorLines = lists:duplicate(CombinedHeight, "  "),
    FullDescWithColor = [?yellow(Title)|[?blue(Line) || Line <- WrappedDesc]],
    FullDescWithExtraLines = case length(FullDescWithColor) < CombinedHeight of
				 true ->
				     NrExtraLines = CombinedHeight - length(FullDescWithColor),
				     FullDescWithColor++lists:duplicate(NrExtraLines, "");
				 false -> FullDescWithColor
			     end,
    FullText = lists:zip3(AreaMapLines, SeparatorLines, FullDescWithExtraLines),
    SendLine = fun({MapLine, SepLine, DescLine}) ->
		       ?send(MapLine++SepLine++DescLine)
	       end,
    lists:foreach(SendLine, FullText).
