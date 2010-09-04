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
-export([loop/2, parse_command/2, send_paragraphs/3]).

-include("telnetcolors.hrl").

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
			    gen_server:call(PlayerPid, enter_default_room),
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
		    ?send(?PROMPT++"Sorry, that does nothing."),
		    loop(Socket, Pid);
		{look, Desc} ->
		    send_paragraphs(Socket, Desc, false),
		    loop(Socket, Pid);
		{unknown, _Command} ->
		    ?send(?PROMPT++"Sorry, that didn't make any sense."),
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
	    send_paragraphs(Socket, [Message], true),
	    loop(Socket, Pid);
	Other ->
	    io:format("tcp_receive_loop: ~p~n", [Other])
    end.

parse_command(Command, Pid) ->
    case Command of
	"quit" -> {close, "Bye"};
	"n" -> player:move("north", Pid);
	"s" -> player:move("south", Pid);
	"w" -> player:move("west", Pid);
	"e" -> player:move("east", Pid);
	"nw" -> player:move("northwest", Pid);
	"ne" -> player:move("northeast", Pid);
	"sw" -> player:move("southwest", Pid);
	"se" -> player:move("southeast", Pid);
	"up" -> player:move("up", Pid);
	"down" -> player:move("down", Pid); %% ugh, there needs to be a better way of doing this.
	"l" -> player:look(Pid);
	"look" -> player:look(Pid);
	_ -> {unknown, Command}
    end.

send_paragraphs(Socket, Paragraphs, Notification) ->
    SendLine = fun(Line, FirstLine) ->
		       case (Notification and FirstLine) of
			   true -> ?send(Line);
			   false -> ?send(?PROMPT++Line)
		       end
	       end,
    SendParagraph = fun(Paragraph) ->
			    Lines = helpers:wrap(Paragraph, 80),
			    helpers:foreachex(SendLine, Lines)
		    end,
    lists:foreach(SendParagraph, Paragraphs).
