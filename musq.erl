%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% This is the 'main' module. This should start every thing else.
%%% aka. This is the file you load when you want to start the game.
%%% @end
%%% Created :  6 Aug 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(musq).
-export([start/0]).

start() ->  %% this will be very temporary. simpleroom.erl needs to go.
            %% we're not going to use that anymore since we'll use gen_server
            %% for al the rooms.
    DefaultRoom = simpleroom:newroom("rooms/source1.room"),
    simpleroom:set_default_room(DefaultRoom),
    connhandler:start_server().
