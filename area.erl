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
    {ok, [RoomPaths|Exits]} = file:consult(Filename), 
    Rooms = dict:from_list(ensure_rooms_loaded(RoomPaths)),
    lists:foreach(fun(X) -> create_exits(Rooms,X) end,Exits).
    

ensure_rooms_loaded(RoomPaths) ->
    lists:map(fun ensure_room_loaded/1, RoomPaths).

ensure_room_loaded({RoomRef, RoomPath}) ->
    RoomName = helpers:room_name_from_filename(RoomPath), 
    case whereis(RoomName) of
        undefined ->
            {ok, _Pid} = room:load(RoomPath), 
            {RoomRef, RoomName};
        _Pid ->
            {RoomRef, RoomName}
    end.


create_exits(RoomsDict,{Room,ExitList}) ->
    RoomPid = dict:fetch(Room,RoomsDict),
    lists:map(
      fun({ExitName,Destination}) ->
	      room:add_exit(RoomPid,ExitName,dict:fetch(Destination,RoomsDict)) end,
      ExitList).


%% for testing:
%% area:load("/home/gert/src/musq/areas/source.area").

