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
		{close, Response} ->
		    gen_tcp:send(Socket, Response);
		{ok, Response, NewState} ->
		    ?send("You tried to "++ ?red(Response) ++" without result."),
		    player_handler(Socket, NewState);
		_ ->
		    ?send("Sorry, that didn't make any sense.")
	    end;
	{tcp_closed, Socket} ->
	    %% question is now, what do we do when the player disconnects in mid-combat?
	    %% perhaps we should answer that when we've actually implemented combat ;-)
	    dbstuff:save_player(self(), State);
	    %% still need to write code for that
	{print, Message} ->
	    gen_tcp:send(Socket, Message)
    end,
    State.

parse_command(Command, State) ->
    case Command of
	"quit" ->
	    {close, "Bye\n\n"};
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
	_ ->
	    {ok, Command, State}
    end.

save(State) ->
    dbstuff:save_player(State).

move(Direction, State) ->
    {ok, "move " ++ Direction, State}.

%% some general stuff should be parsed, nothing more.
%% basicly this module only needs to accept 'print' events for stuff that needs to be
%% written to the player's screen
%% and it needs to accept the incoming stuff from the socket.
%% from that, the module needs to decide where the message should be sent.
%% to the room, to another player, an NPC?,.. etc
%% all other logic should be in other modules
