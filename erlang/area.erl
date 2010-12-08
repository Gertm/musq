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
-record(tile, {x				::integer(),
			   y				::integer(),
			   images			::[string()],
			   properties		::[term()]
			  }).
-record(area, {name				::string(),
			   width			::integer(),
			   height			::integer(),
			   defaulttile		::#tile{},
			   bordertile		::#tile{},
			   tiles			::[#tile{}],
			   playerpids		::[pid()]
			  }).

start(_FileName) ->
	ok.

loop(_AreaState) ->
	ok.

-spec(load(FileName::string()) -> term()).
load(FileName) ->
    mochijson:decode(readlines(FileName)).
	
-spec(readlines(FileName::string()) -> string()).
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    lists:flatten(get_all_lines(Device, [])).

%% could optimize this further to use binaries.
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.


%% for future reference on using Eunit. (it's been a while..)
dummy_adder(X,Y) ->
	X + Y.

dummy_adder_test() ->
	?assertEqual(dummy_adder(4,5), 9),
	?assertEqual(dummy_adder(2,3), 5).
