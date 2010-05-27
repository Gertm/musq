%%% @author Gert M <cel357@gmail.com>
%%% @copyright (C) 2010, Gert M
%%% @doc
%%% An area defines a group of rooms.
%%% This module loads the .area files into the game
%%% Should be called from the world when that world loads.
%%% @end
%%% Created : 24 May 2010 by Gert M <cel357@gmail.com>

-module(area).
-compile(export_all).  %% for now

%% area file = erlang list describing the layout of the rooms.
%% load area file into dict
%% add appropriate exits to the rooms
%% spawn all the rooms needed.

load(Filename) ->
    {ok,RoomList} = file:consult(Filename),
    RoomList.

%% for testing:
%% area:load("/home/gert/src/explore/erlang/musq/areas/source.area").
