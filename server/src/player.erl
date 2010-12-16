-module(player).
-include("musq.hrl").
-compile(export_all).

-record(plr, {name = "UnnamedPlayer" ::string(),
			  area ::atom()}).

start(WsPid) ->
	spawn_link(?MODULE, loop, [WsPid, #plr{}]).

playerstart(WsPid,PlayerState) ->
	process_flag(trap_exit,true),
	loop(WsPid,PlayerState).

loop(WsPid,PlayerState) ->
	receive
		{login, From, Login} ->
			From ! Login,
			loop(WsPid, PlayerState);
		{'EXIT', _Pid, Reason} ->
			io:format("Eep! ~s~n",Reason)
	end.

