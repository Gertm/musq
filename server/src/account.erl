%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Account module. Contains the code to check for/create accounts.
%%% @end
%%% Created : 19 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(account).
-include("musq.hrl").

-compile(export_all).

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
	
			
get_visual_from_account(#account{}=Acc) ->
	ViList = [ hlp:visual_image_to_struct(X) || X <- Acc#account.images ],
	{"Visual", {struct,[{"Name", Acc#account.username},
						{"Images", {array, ViList}}]}}.

visual_part(AccountName) ->
	get_visual_from_account(db:dirty_read_account(AccountName)).

create_account_nx(RequestParams) ->
	AR = get_account_record(RequestParams),
	case db:account_exists(AR#account.username) of
		true ->
			hlp:create_reply("createAccount",
							[{"Success","false"},
							 {"Reason","Account already exists"}]);
		false ->
			db:write_account(AR),
			hlp:create_reply("createAccount",
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
