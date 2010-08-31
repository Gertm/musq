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

player_handler(Socket, PlayerState) ->
    gen_tcp:send(Socket, "#> "),
    case helpers:recv_string(Socket) of
	{tcp, Socket, Cmd} ->
	    case parse_command(Cmd, PlayerState) of
		{close, Response} ->
		    gen_tcp:send(Socket, Response);
		{ok, Command, NewPlayerState} ->
		    ?send("#> You "++ ?green(Command) ++"."),
		    player_handler(Socket, NewPlayerState);
		{unknown, Command} ->
		    ?send("#> You tried to "++ ?red(Command) ++" without result."),
		    player_handler(Socket, PlayerState);
		_ ->
		    ?send("#> Sorry, that didn't make any sense."),
		    player_handler(Socket, PlayerState)
	    end;
	{tcp_closed, Socket} ->
	    %% question is now, what do we do when the player disconnects in mid-combat?
	    %% perhaps we should answer that when we've actually implemented combat ;-)
	    dbstuff:save_player(self(), PlayerState);
	    %% still need to write code for that
	{room_entered, RoomPid, RoomName} ->
	    ?send("You entered room "++ ?green(RoomName) ++"."),
	    NewPlayerState = PlayerState#player{room=RoomPid},
	    player_handler(Socket, NewPlayerState);
	{print, Message} ->
	    ?send(Message),
	    player_handler(Socket, PlayerState)
    end,
    PlayerState.

parse_command(Command, PlayerState) ->
    case Command of
	"quit" ->
	    {close, "Bye\n\n"};
	"n" -> move("north", PlayerState);
	"s" -> move("south", PlayerState);
	"w" -> move("west", PlayerState);
	"e" -> move("east", PlayerState);
	"nw" -> move("northwest", PlayerState);
	"ne" -> move("northeast", PlayerState);
	"sw" -> move("southwest", PlayerState);
	"se" -> move("southeast", PlayerState);
	"up" -> move("up", PlayerState);
	"down" -> move("down", PlayerState); %% ugh, there needs to be a better way of doing this.
	"l" -> look(PlayerState);
	"look" -> look(PlayerState);
	_ -> {error, Command}
    end.

save(PlayerState) ->
    dbstuff:save_player(PlayerState).

move(Direction, PlayerState) ->
    {unknown, "move " ++ Direction}.

look(PlayerState) ->
    Room = PlayerState#player.room,
    Room ! {look, self()},
    {ok, "look", PlayerState}.

%% some general stuff should be parsed, nothing more.
%% basicly this module only needs to accept 'print' events for stuff that needs to be
%% written to the player's screen
%% and it needs to accept the incoming stuff from the socket.
%% from that, the module needs to decide where the message should be sent.
%% to the room, to another player, an NPC?,.. etc
%% all other logic should be in other modules
