%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Account module. Contains the code to check for/create accounts.
%%% @end
%%% Created : 19 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(account).
-include("musq.hrl").

-compile(export_all).

table_exists() ->
	lists:any(fun(X) -> X == accounts end, mnesia:system_info(tables)).

create_table() ->
	mnesia:create_table(account, [{type, set},
								  {disc_copies,[node()]},
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
	read_helper(AccList).

dirty_read_account(AccountName) ->
	AccList = mnesia:dirty_read({account, AccountName}),
	read_helper(AccList).

read_helper(AccList) ->
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
			
get_visual_from_account(#account{}=Acc) ->
	hlp:createReply("visual",
					[{"Name",Acc#account.username},
					 {"Images",{array,Acc#account.images}}]).

create_account_nx(RequestParams) ->
	AR = account:get_account_record(RequestParams),
	case account_exists(AR#account.username) of
		true ->
			hlp:createReply("createAccount",
							[{"Success","false"},
							 {"Reason","Account already exists"}]);
		false ->
			write_account(AR),
			hlp:createReply("createAccount",
							[{"Success","true"}])
	end.



%%% stuff to remind me how these request look like:
%%% {
%%   "Functions": "createAccount",
%%   "Params":
%%   {
%%         "Success": "true/false",
%%         "Reason": "why creating your account failed"
%%   }
%% }

%% {
%%   "Functions": "visual",
%%   "Params":
%%   {
%%     "Name": "name",
%%     "Images":
%%       [
%%         {
%%           "Url": "images/faces/..."
%%         },
%%         {
%%           "Url": "images/faces/...",
%%           "Color": "#..."
%%         },
%%       ]
%%   }
%% }
