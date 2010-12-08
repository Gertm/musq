%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2010 by Gert <@G3rtm on Twitter>

-module(area).
-include("musq.hrl").
-compile(export_all).

%% record definitions -- nobody needs these except for this module
-record(area, {name, width, height, defaulttile, bordertile, tiles, playerpids}).
-record(tile, {x, y, images, properties}).

start(FileName) ->
	ok.


loop(AreaState) ->
	ok.


load(FileName) ->
	A = lists:flatten(readlines(FileName)),
	mochijson:decode(A).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    mochijson:decode(lists:flatten(get_all_lines(Device, []))).

%% could optimize this further to use binaries.
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.
