%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Module to handle the logging in/out of users.
%%% @end
%%% Created : 23 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(login).
-include("musq.hrl").
-compile(export_all).

is_logged_in(PlayerName) ->
	Loggedin = player:read_player(PlayerName),
	case Loggedin of
		{atomic, []} ->
			no_user;
		{atomic, #plr{}=P} ->
			P#plr.logged_in
	end.

check_pwd(PlayerName, Password) ->
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

log_in_out(PlayerName,TrueFalse) ->
	F = fun() -> {atomic, #plr{}=Player} = mnesia:read(player,PlayerName),
				 mnesia:write(player, Player#plr{logged_in = TrueFalse}) end,	
	mnesia:transaction(F).


success(PlayerName,Password) ->
	case check_pwd(PlayerName, Password) of
		{error, Reason} -> 
			{error, hlp:create_reply("login",[{"Success","false"},{"Reason",Reason}])};
		ok -> 
			{ok, hlp:create_reply("login",[{"Success","true"}])}
	end.

logout(PlayerName) ->		
	log_in_out(PlayerName, false).

login(PlayerName, Password, PlayerPid) ->
	case success(PlayerName,Password) of
		{error, Reply} ->
			player:relay(PlayerPid, Reply);
		{ok, Reply} ->
			player:relay(PlayerPid, Reply),
			V = account:visual_request(PlayerName),
			player:relay(PlayerPid, V)
	end.
			
			%% notify the area so it can send:
			%% - area definition
			%% - jump request
	
