-module(connhandler).
%% initially got this from http://www.joeandmotorboat.com/2008/11/12/a-simple-concurrent-erlang-tcp-server/
-export([start_server/0, connect/1, recv_loop/1, restart/0]).

-define(LISTEN_PORT, 5701).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, true}]).

start_server() ->
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
	{ok, Listen} -> spawn(?MODULE, connect, [Listen]),
			io:format("~p Server Started.~n", [erlang:localtime()]),
			register(main_server,Listen);
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

connect(Listen) ->
    io:format("Listening for new connections: ~p~n",[self()]),
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
    spawn(fun() -> connect(Listen) end),
    recv_loop(Socket).

recv_loop(Socket) ->
    io:format("~p",[self()]),
    motd:motd(Socket),
    gen_tcp:send(Socket, "\r\n\nPlease enter your username: "),
    receive
	{tcp, Socket, Data} ->
	    io:format("User ~s logging in..",[binary_to_list(Data)]),
	    gen_tcp:send(Socket, "\nPassword: ");
	{tcp_closed, Socket} ->
	    io:format("~p Client Disconnected.~n", [erlang:localtime()])
    end,
    receive
	{tcp, Socket, Pwd} ->
	    case binary_to_list(Pwd) of
		"quit\n" ->
		    {exit,"got quit command"};
		Password ->
		    gen_tcp:send(Socket,"Password accepted! Sending you to your starting location.\n")
	    end,
	    Pid = spawn(player,player_handler,[Socket,[]]),
	    gen_tcp:controlling_process(Socket,Pid);
	{tcp_closed,Socket} -> io:format("~p Client Disconnected.~n", [erlang:localtime()])
    end.



