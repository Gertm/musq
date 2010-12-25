%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Grouping all the DB functions.
%%% @end
%%% Created : 25 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(db).
-include("musq.hrl").
-compile(export_all).


%% PLAYER stuff
read_player(PlayerName) ->
	mnesia:transaction(fun() -> mnesia:read(player, PlayerName) end).

dirty_read_player(PlayerName) ->
	[#plr{} = P] = mnesia:dirty_read({player,PlayerName}),
	P.

player_by_name(PlayerName) ->
	read_player(PlayerName).

player_by_pid(Pid) ->
	[P] = mnesia:dirty_index_read(plr,Pid,#plr.pid),
	P.

prepare_database() ->
	account:create_table(),
	mnesia:create_table(player, [{disc_copies, [node()]},
								 {type, set},
								 {attributes, record_info(fields, plr)},
								 {index, [position, area, pid]}]).

%% ACCOUNT stuff
create_account_table() ->
	mnesia:create_table(account, [{type, set},
								  {disc_copies,[node()]},
								  {attributes, record_info(fields, account)}]).

write_account(AR) ->
	mnesia:transaction(fun() -> mnesia:write(AR) end).

read_account(AccountName) ->							   
	{atomic, AccList} = mnesia:transaction(fun() -> mnesia:read({account, AccountName}) end),
	acc_read_helper(AccList).

dirty_read_account(AccountName) ->
	AccList = mnesia:dirty_read({account, AccountName}),
	acc_read_helper(AccList).

acc_read_helper(AccList) ->
	case AccList of
		[] -> {error, no_user};
		[#account{} = A] -> A
	end.
	

account_exists(AccountName) ->
	case read_account(AccountName) of
		{error, no_user} ->
			false;
		_ ->
			true
	end.


%% LOGIN stuf

log_in_out(PlayerName,TrueFalse) ->
	F = fun() -> {atomic, #plr{}=Player} = mnesia:read(player,PlayerName),
				 mnesia:write(player, Player#plr{logged_in = TrueFalse}) end,	
	mnesia:transaction(F).
