%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Helper functions
%%% @end
%%% Created : 15 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(hlp).
-include("musq.hrl").

-compile(export_all).

kv(K,L) ->
	{K,V} = lists:keyfind(K,1,L),
	V.

create_reply(Func,Params) ->
	{struct, [{"Function",Func},
			  {"Params",
			   {struct,Params}}]}.

%%% @doc
%%% takes the param list of a getFiles request and returns the result term,
%%% @end
getFiles(Params) ->
	OriginalBasePath = kv("BasePath", Params),
	BasePath = ?BASEPATH ++ "client/" ++ OriginalBasePath,
	WildCard = kv("WildCard", Params),
	FileList = filelib:wildcard(WildCard,BasePath),
	NewFL = lists:map(fun(X) -> OriginalBasePath ++ X end, FileList),
	create_reply("getFiles",[{"Images",{array,NewFL}}]).
	%% {struct,[{"Functions","getFiles"},
	%% 		 {"Params",
	%% 		  {struct,[{"Images",
	%% 					{array,FileList}}]}}]}.

-spec(load_json(FileName::string()) -> term()).
%% @spec -spec(load(FileName::string()) -> term()
load_json(FileName) ->
    mochijson:decode(readlines(FileName)).
	
-spec(readlines(FileName::string()) -> string()).
%% @spec readlines(FileName::string()) -> string()
readlines(FileName) ->
	erlang:display("Going to open: "++FileName),
    {ok, Device} = file:open(FileName, [read]), 
    lists:flatten(get_all_lines(Device, [])).

%% could optimize this further to use binaries.
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.

visual_image_to_struct(#visualImage{}=V) ->
	{struct, [{"Url",V#visualImage.url},
			  {"Color", V#visualImage.color}]}.
   
