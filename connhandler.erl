-module(connhandler).
%% initially got this from http://www.joeandmotorboat.com/2008/11/12/a-simple-concurrent-erlang-tcp-server/
-export([start_server/0, restart/0, connect/1, recv_login/1]).
-include("records.hrl").
-include("telnetcolors.hrl").
-define(LISTEN_PORT, 5701).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, once}]).

start_server() ->
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
	{ok, Listen} -> spawn(?MODULE, connect, [Listen]),
			io:format("~p Server Started.~n", [erlang:localtime()]),
			register(musq_con_listener, Listen);
	Error ->
	    io:format("Error: ~p~n", [Error])
    end.

restart() ->
    case whereis(main_server) of
	undefined ->
	    not_running;
	_ ->
	    main_server ! {'EXIT',self(),"Stop was requested."}
    end,
    start_server().

recv_login(Socket) ->
    io:format("~p", [self()]),
    motd:motd(Socket),
    gen_tcp:send(Socket, "Please enter your username: "),
    case helpers:recv_string(Socket) of
	{tcp, Socket, Username} ->
	    io:format("User ~s logging in.~n", [Username]),
	    gen_tcp:send(Socket, "Password: "),
	    case helpers:recv_string(Socket) of
		{tcp, Socket, "quit"} ->
		    {exit, "got quit command"};
		{tcp, Socket, Password} ->
		    io:format("Password ~s received.~n", [Password]),
		    Player = #player{name=Username},
		    PlayerPid = spawn(player, player_handler, [Socket, Player]),
		    gen_tcp:controlling_process(Socket, PlayerPid),
		    ?send("Password accepted! Sending you to your starting location."),
		    RoomPid = simpleroom:get_default_room(),
		    RoomPid ! {enter, PlayerPid};
		_ -> {exit, "connection lost"}
	    end;
	_ -> {exit, "connection lost"}
    end.

connect(Listen) ->
    io:format("Listening for new connections: ~p~n", [self()]),
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
    spawn(fun() -> connect(Listen) end),
    recv_login(Socket).
