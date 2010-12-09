%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert
%%% @doc
%%% The area module.
%%% This could probably be converted to be an OTP gen_server, but I'm not sure yet.
%%% Once I learn more about how OTP and gen_server work, I'll probably port it.
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
			   playerpids		::[pid()],
			   world			::pid()
			  }).
%% the area record should keep track of the exits to other areas and those areas
%% exits could be a property in the tile record, but getting the neighbouring areas
%% could be a bit difficult. The area loading code could scan the area definition once
%% and check for exits there.
%% Alternatively, the area could notify the world about the player leaving the area and going to
%% the area with 'name' (atom()). Then there should only be one world?
%% or multiple worlds and 1 universe? :-)  How many dimensions are there again? ;-P

start(_FileName) ->
	ok.

loop(_AreaState) ->
	receive
		{enter, _Pid} ->
			%% this needs to return the initial coordinates of the player in the area
			%% notify the other players in the same area
			ok;
		{leave, _Pid} ->
			%% notify the other players in the same area
			ok;
		{move, _Pid} ->
			%% check whether the move is possible,
			%% return the new coordinates of the player.
			%% notify the other players in the same area
			ok;
		{talk, _Pid} ->
			%% notify the other players in the same area
			ok
	end.


-spec(load(FileName::string()) -> term()).
%% @spec -spec(load(FileName::string()) -> term()
load(FileName) ->
    mochijson:decode(readlines(FileName)).
	
-spec(readlines(FileName::string()) -> string()).
%% @spec readlines(FileName::string()) -> string()
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    lists:flatten(get_all_lines(Device, [])).

%% could optimize this further to use binaries.
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.

broadcast(Message, PlayerPids) ->
	[ Pid ! Message || Pid <- PlayerPids ].

%% for future reference on using Eunit. (it's been a while..)
dummy_adder(X,Y) ->
	X + Y.

dummy_adder_test() ->
	?assertEqual(dummy_adder(4,5), 9),
	?assertEqual(dummy_adder(2,3), 5).
