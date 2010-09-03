-module(connhandler).
%% initially got this from http://www.joeandmotorboat.com/2008/11/12/a-simple-concurrent-erlang-tcp-server/
%% interface
-export([start_server/0, stop_server/0, restart_server/0]).
%% helpers
-export([connect/1]).
-include("records.hrl").
-include("telnetcolors.hrl").
-define(LISTEN_PORT, 5701).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, once}]).

start_server() ->
    case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
	{ok, Listen} -> spawn(?MODULE, connect, [Listen]),
			io:format("~p Server Started.~n", [erlang:localtime()]),
			register(?MODULE, Listen);
	Error ->
	    io:format("Error: ~p~n", [Error])
    end.

stop_server() ->
    case whereis(?MODULE) of
	undefined ->
	    not_running;
	_ ->
	    ?MODULE ! {'EXIT', self(), "Stop was requested."},
	    ok
    end.

restart_server() ->
    case stop_server() of
        ok -> start_server();
	Other -> Other
    end.

connect(Listen) ->
    io:format("Listening for new connections: ~p~n", [self()]),
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, ?TCP_OPTS),
    spawn(fun() -> connect(Listen) end),
    playercontroller:handle(Socket).
