%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2010 by Gert <@G3rtm on Twitter>

-module(area).
-include("musq.hrl").
-compile(export_all).

start(FileName) ->
	ok.


loop(AreaState) ->
	ok.


load(FileName) ->
	A = lists:flatten(readlines(FileName)),
	mochijson:decode(A).


readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.
