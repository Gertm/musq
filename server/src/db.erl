%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Grouping all the DB functions.
%%% @end
%%% Created : 25 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(db).
-include("musq.hrl").
-compile(export_all).


get_transaction_result({atomic, R}) ->
	case R of
		[] -> [];
		[_|_] when length(R) > 1 -> R;
		[Result] -> Result
	end.

%% PLAYER stuff
read_player(Pid) when is_pid(Pid) ->
	R = mnesia:transaction(fun() -> mnesia:index_read(plr, Pid, #plr.pid) end), 
	get_transaction_result(R);
read_player(PlayerName) ->
	R = mnesia:transaction(fun() -> mnesia:read(player, PlayerName) end), 
	get_transaction_result(R).

dirty_read_player(Pid) when is_pid(Pid) ->
	mnesia:dirty_index_read(plr, Pid, #plr.pid);
dirty_read_player(PlayerName) ->
    mnesia:dirty_read({player, PlayerName}).

create_player_record(PlayerName, PlayerPid) -> 
	P = dirty_read_player(PlayerName), 
	case P of
		[] -> 
			F = fun() -> mnesia:write(player, #plr{name=PlayerName, pid=PlayerPid}) end, 
			mnesia:transaction(F);
		_ -> ok
	end.
							   

prepare_database() ->
	create_account_table(), 
	mnesia:create_table(player, [{disc_copies, [node()]}, 
								 {type, set}, 
								 {attributes, record_info(fields, plr)}, 
								 {index, [position, area, pid]}]).

%% ACCOUNT stuff
create_account_table() ->
	mnesia:create_table(account, [{type, set}, 
								  {disc_copies, [node()]}, 
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

log_in_out(PlayerName, TrueFalse) ->
	F = fun() -> {atomic, #plr{}=Player} = mnesia:read(player, PlayerName), 
				 mnesia:write(player, Player#plr{logged_in = TrueFalse}) end, 	
	mnesia:transaction(F).
