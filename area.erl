%%% @author Gert M <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert M
%%% @doc
%%% An area defines a group of rooms.
%%% This module loads the .area files into the game
%%% Should be called from the world when that world loads.
%%% @end
%%% Created : 24 May 2010 by Gert M

-module(area). 
-compile(export_all).  %% for now

%% area file = erlang list describing the layout of the rooms.
%% load area file into dict
%% add appropriate exits to the rooms
%% spawn all the rooms needed.
%% the rooms loaded have the form: {{x, y, z}, "roomfile"}
%% where roomfile is the filename of the room
%% it would be handy if the room filename always has to be
%% roomname.room  That way it'll be easier to do lookups in the
%% global process registry. (to know if the room is already spawned
%% or not.)

load(Filename) ->
    {ok, RoomList} = file:consult(Filename).
    

%% for testing:
%% area:load("/home/gert/src/musq/areas/source.area").

