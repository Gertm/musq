%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Account module. Contains the code to check for/create accounts.
%%% @end
%%% Created : 19 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(account).
-include("musq.hrl").

-compile(export_all).

-record(account, {username ::string(),
				  password ::string(),
				  email    ::string(),
				  images   ::[#visualImage{}]}).

table_exists() ->
	lists:any(fun(X) -> X == accounts end, mnesia:system_info(tables)).

create_table() ->
	mnesia:create_table(account, [{type, set},
								   {attributes, record_info(fields, account)}]).

get_account_record(RequestParams) ->
	[{"Username",Username},
	 {"Password",Password},
	 {"Email",Email},
	 {"Images",{array,ImgList}}] = RequestParams,
	AccRec = #account{username = Username,
					  password = Password,
					  email = Email},
	Images = lists:map(fun(X) ->
							   case X of
								   {struct, [{"Url",Url},
											 {"Color", Color}]} ->
									   #visualImage{url=Url,color=Color};
								   {struct, [{"Url",Url}]} ->
									   #visualImage{url=Url,color="#000000"}
							   end
					   end, ImgList),
	AccRec#account{images = Images}.
	
write_account(AR) ->
	mnesia:transaction(fun() -> mnesia:write(AR) end).


read_account(AccountName) ->							   
	{atomic, AccList} = mnesia:transaction(fun() -> mnesia:read({account, AccountName}) end),
	case AccList of
		[] -> {error, no_user};
		[#account{} = A] -> A
	end.

												   
