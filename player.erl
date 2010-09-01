%%% @author Gert M <cel357@gmail.com>
%%% @copyright (C) 2010, Gert M
%%% @doc
%%% The module for the player interactions
%%% @end
%%% Created : 26 Apr 2010 by Gert M <cel357@gmail.com>

%% perhaps this module should also become a gen_server
%% should be easier to work with when things go bad.

-module(player).
-compile(export_all).
-include("telnetcolors.hrl").
-include("records.hrl").

player_handler(Socket, State) ->
    gen_tcp:send(Socket, "#> "),
    case helpers:recv_string(Socket) of
	{tcp, Socket, Cmd} ->
	    case parse_command(Cmd, State) of
		ok ->
		    ?send("#> Sorry, that does nothing."),
		    player_handler(Socket, State);
		{look, Desc} ->
		    [ ?send("#> "++DescLine) || DescLine <- Desc ],
		    player_handler(Socket, State);
		{unknown, _Command} ->
		    ?send("#> Sorry, that didn't make any sense."),
		    player_handler(Socket, State);
		{close, Response} ->
		    gen_tcp:send(Socket, Response);
		Other ->
		    io:format("player_handler/recv_string/parse_command: ~p~n", [Other])
	    end;
	{tcp_closed, Socket} ->
	    %% question is now, what do we do when the player disconnects in mid-combat?
	    %% perhaps we should answer that when we've actually implemented combat ;-)
	    dbstuff:save_player(self(), State); %% > still need to write code for that
	%% temporary
	enter_default_room ->
	    DefaultRoom = source1,
	    room:enter(DefaultRoom, "space"),
	    NewState = State#player{room=DefaultRoom},
	    gen_tcp:send(Socket, "You've entered the default room.\r\n"),
	    player_handler(Socket, NewState);
	{roommsg, Message} ->
	    gen_tcp:send(Socket, Message++"\r\n"),
	    player_handler(Socket, State);
	Other ->
	    io:format("player_handler/recv_string: ~p~n", [Other])
    end,
    State.

parse_command(Command, State) ->
    case Command of
	"quit" -> {close, "Bye\n\n"};
	"n" -> move("north", State);
	"s" -> move("south", State);
	"w" -> move("west", State);
	"e" -> move("east", State);
	"nw" -> move("northwest", State);
	"ne" -> move("northeast", State);
	"sw" -> move("southwest", State);
	"se" -> move("southeast", State);
	"up" -> move("up", State);
	"down" -> move("down", State); %% ugh, there needs to be a better way of doing this.
	"l" -> look(State);
	"look" -> look(State);
	_ -> {unknown, Command}
    end.

save(State) ->
    dbstuff:save_player(State).

move(Direction, State) ->
    Room = State#player.room,
    room:move(Room, Direction).

look(State) ->
    Room = State#player.room,
    room:look(Room).

%% some general stuff should be parsed, nothing more.
%% basicly this module only needs to accept 'print' events for stuff that needs to be
%% written to the player's screen
%% and it needs to accept the incoming stuff from the socket.
%% from that, the module needs to decide where the message should be sent.
%% to the room, to another player, an NPC?,.. etc
%% all other logic should be in other modules
