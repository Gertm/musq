%%% @author Gert M <cel357@gmail.com>
%%% @copyright (C) 2010, Gert M
%%% @doc
%%% The module for the player interactions
%%% @end
%%% Created : 26 Apr 2010 by Gert M <cel357@gmail.com>

-module(player).
-compile(export_all).
-include("telnetcolors.hrl").

clean_tcp_input(TcpInput) ->
    Len = erlang:round(bit_size(TcpInput)/8),
    {Out,_} = split_binary(TcpInput,Len-2),
    binary_to_list(Out).

player_handler(Socket,State) ->
    gen_tcp:send(Socket,"#> "), %% this macro is also in telnetcolors.hrl
    inet:setopts(Socket,[{active,once}]),
    receive
	{tcp, Socket, Command} ->
	    Cmd = clean_tcp_input(Command),
	    case parse_command(Cmd,State) of
		{close, Response} ->
		    gen_tcp:send(Socket,Response);
		{ok, Response, NewState} ->
		    ColorRes = ?red(Response),
		    ?send("You tried to "++ ColorRes ++" without result."),
		    player_handler(Socket,NewState);
		_ ->
		    gen_tcp:send(Socket,"Sorry, that didn't make any sense.")
	    end;
	{tcp_closed, Socket} ->
	    %% save stuff if we need to
	    %% still need to write code for that
	    io:format("Client disconnected!");
	{print, Message} ->
	    gen_tcp:send(Socket,Message)
    end,
    State.

parse_command(Command,State) ->
    case Command of
	"quit" ->
	    {close,"Bye\n\n"};
	"n" -> move("north",State);
	_ ->
	    {ok, Command, State}
    end.

move(_Direction,_State) ->
    ok.
%% some general stuff should be parsed, nothing more.
%% basicly this module only needs to accept 'print' events for stuff that needs to be
%% written to the player's screen
%% and it needs to accept the incoming stuff from the socket.
%% from that, the module needs to decide where the message should be sent.
%% to the room, to another player, an NPC?,.. etc
%% all other logic should be in other modules
