-module(player).
-include("musq.hrl").

-export([start/1,loop/2]).

start(WsPid) ->
	?InfoMsg("Spawning new player~n",[]),
	spawn_link(?MODULE, loop, [WsPid, #plr{}]).

loop(WsPid,PlayerState) ->
	receive
		{login, WsPid, Login} ->
			WsPid ! {reply, self(), Login},
			loop(WsPid, PlayerState);
		{getFiles, _From, Params} ->
			?InfoMsg("getFiles params: ~p~n",[Params]),
			case Params of
				[] -> ok;
				_ -> R = hlp:getFiles(Params),
					 ?InfoMsg("Sending back to wshandle: ~p~n",[R]),
					 WsPid ! {reply, self(), R}
			end,
			loop(WsPid, PlayerState);
		{createAccount, _From, Params} ->
			%% when the player is logged in, this shouldn't work.
			?InfoMsg("handling createaccount~n",[]),
			R = gen_server:call(world, {createAccount, Params}),
			WsPid ! {reply, self(), R},
			loop(WsPid,PlayerState);
		{keepalive, _From, []} ->
			WsPid ! {reply, self(), hlp:createReply("keepalive",[])},
			loop(WsPid, PlayerState);
		{'EXIT', _Pid, Reason} ->
			io:format("Eep! ~s~n",Reason);
		Any -> ?InfoMsg("no idea what this is: ~p~n",[Any]), 
			loop(WsPid, PlayerState)
	end.


read_player(PlayerName) ->
	mnesia:transaction(fun() -> mnesia:read(player, PlayerName) end).

is_logged_in(PlayerName) ->
	Loggedin = read_player(PlayerName),
	case Loggedin of
		{atomic, []} ->
			no_user;
		{atomic, #plr{}=P} ->
			P#plr.logged_in
	end.

user_login(PlayerName, Password) ->
	case is_logged_in(PlayerName) of
		no_user ->
			{error, "no such user"};
		true ->
			{error, "already logged in"};
		false ->
			A = account:read_account(PlayerName),
			if
				A#account.password == Password ->
					
					ok;
				true ->
					{error, "incorrect password"}
			end
	end.

log_in_user(PlayerName,TrueFalse) ->
	{atomic, #plr{}=Player} = read_player(PlayerName),
	mnesia:transaction(fun() -> mnesia:write(player, Player#plr{logged_in = TrueFalse}) end).
