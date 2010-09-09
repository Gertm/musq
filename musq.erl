%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% This is the 'main' module. This should start every thing else.
%%% aka. This is the file you load when you want to start the game.
%%% @end
%%% Created :  6 Aug 2010 by Gert Meulyzer <@G3rtm on Twitter>

-module(musq).
-export([start/0]).

start() ->
    %% temporary stuff
    dbstuff:start(),
    case area:load("areas/source.area") of
	ok ->
            connhandler:start_server();
	_ -> error
    end.
