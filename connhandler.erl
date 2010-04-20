-module(connhandler).
%% initially got this from http://www.joeandmotorboat.com/2008/11/12/a-simple-concurrent-erlang-tcp-server/
-export([start_server/0, connect/1, recv_loop/1, player_handler/2,clean_tcp_input/1]).

-define(LISTEN_PORT, 5701).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, true}]).


start_server() ->
    %% start up the service and error out if we cannot
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
	{ok, Listen} -> spawn(?MODULE, connect, [Listen]),
			io:format("~p Server Started.~n", [erlang:localtime()]);
	Error ->
	    io:format("Error: ~p~n", [Error])
    end.

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
    spawn(fun() -> connect(Listen) end),
    recv_loop(Socket).
 %% gen_tcp:close(Socket).

recv_loop(Socket) ->
    io:format("~p",[self()]),
    gen_tcp:send(Socket, "Welcome to M.U.S.Q.\n"),
    gen_tcp:send(Socket, "Please enter your username: "),
    receive
	{tcp, Socket, Data} ->
	    io:format("User ~s logging in..",[binary_to_list(Data)]),
	    %% inet:setopts(Socket,[{active, once}]),
	    gen_tcp:send(Socket, "\nPassword: ");
	{tcp_closed, Socket} ->
	    io:format("~p Client Disconnected.~n", [erlang:localtime()])
    end,
    receive
	{tcp, Socket, Pwd} ->
	    %% inet:setopts(Socket,[{active,once}]),
	    case binary_to_list(Pwd) of
		"quit\n" ->
		    {exit,"got quit command"};
		Password ->
		    gen_tcp:send(Socket,"Password accepted! Sending you to your starting location.\n")
	    end,
	    io:format("Before spawning the process"),
	    Pid = spawn(?MODULE,player_handler,[Socket,[]]),
	    io:format("Spawned the process"),
	    gen_tcp:controlling_process(Socket,Pid),
	    io:format("Gave control to the other PID");
	{tcp_closed,Socket} -> io:format("~p Client Discon.~n", [erlang:localtime()])
    end.

clean_tcp_input(TcpInput) ->
    Len = erlang:round(bit_size(TcpInput)/8),
    {Out,_} = split_binary(TcpInput,Len-2),
    binary_to_list(Out).

player_handler(Socket,State) ->
    gen_tcp:send(Socket,"prompt>"),
    inet:setopts(Socket,[{active,once}]),
    receive
	{tcp, Socket, Command} ->
	    Cmd = clean_tcp_input(Command),
	    io:format("~p~n",[Cmd]),
	    gen_tcp:send(Socket,"You've tried to do ["++Cmd++"]\n"),
	    player_handler(Socket,State);
	{tcp_closed, Socket} ->
	    %% save stuff if we need to
	    %% <insert code>
	    io:format("Client disconnected!")
    end,
    State.


