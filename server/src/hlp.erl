%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Helper functions
%%% @end
%%% Created : 15 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(hlp).

-compile(export_all).

kv(K,L) ->
	{K,V} = lists:keyfind(K,1,L),
	V.
