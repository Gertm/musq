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
%% the rooms loaded have the form: {{x,y,z},"roomfile"}
%% where roomfile is the filename of the room

load(Filename) ->
    {ok,RoomList} = file:consult(Filename),
    PosPidPairs = [ {Pos,just_the_pid(room:load(X))} || {Pos, X} <- RoomList ],
    

just_the_pid({ok, Pid}) ->
    Pid.

is_exit_of({{X1, Y1, Z1}, Pid1},{{X2, Y2, Z2}, Pid2}) ->
    ok.
    


%% for testing:
%% area:load("/home/gert/src/musq/areas/source.area").

