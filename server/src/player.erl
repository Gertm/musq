-module(player).
-include("musq.hrl").

-export([start/1,loop/2]).

start(WsPid) ->
	?InfoMsg("Spawning new player~n",[]),
	spawn_link(?MODULE, loop, [WsPid, #plr{}]).

loop(WsPid,PlayerState) ->
	receive
		{login, From, Login} ->
			?InfoMsg("Got login request: ~p~n",[Login]),
			From ! {reply, self(), Login},
			loop(WsPid, PlayerState);
		{getFiles, From, Params} ->
			?InfoMsg("getFiles params: ~p~n",[Params]),
			R = hlp:getFiles(Params),
			?InfoMsg("Sending back to wshandle: ~p~n",[R]),
			WsPid ! {reply, self(), R},
			loop(WsPid, PlayerState);
		{keepalive, _From, []} ->
			WsPid ! {reply, self(), hlp:createReply("keepalive",[])},
			loop(WsPid, PlayerState);
		{'EXIT', _Pid, Reason} ->
			io:format("Eep! ~s~n",Reason);
		Any -> ?InfoMsg("no idea what this is: ~p~n",[Any]), 
			loop(WsPid, PlayerState)
	end.

