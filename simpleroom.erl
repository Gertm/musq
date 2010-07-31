%%% @author Gert  Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert  Meulyzer
%%% @doc
%%% Simple room.
%%% Going to try and make a room without OTP first, so I fully understand everything that is happening.
%%% Later this will be ported to a gen_server so we can use all the OTP stuff.
%%% @end
%%% Created : 28 Jul 2010 by Gert  Meulyzer <@G3rtm on Twitter>

-module(simpleroom).

-compile(export_all).

-record(room, {name,
	       exits=[],
	       desc=[],
	       npcs=[],
	       objects=[],
	       players=[],
	       messages=["room msg 1","room msg 2"]
	      }).

newroom() ->
    spawn(?MODULE,init,[]).

getmsg() ->
    receive
	X -> X
    after 1000 ->
	nothing_received
    end.	

init() ->
    loop(#room{name="testroom1"}).

loop(State) ->
    receive
	{enter,PlayerPid} ->
	    Players = State#room.players,
	    NewPlayers = [PlayerPid|Players],
	    NewState = State#room{players=NewPlayers},
	    PlayerPid ! {ok,State#room.name},
	    io:format("~s~n",["looping from enter"]),
	    loop(NewState);
	{stop} ->
	    io:format("~s~n",["Room killed!"]),
	    {ok,stopped};
	_ -> ok
    after 15000 ->
	    room_message(State),
	    io:format("~s~n",["looping from after"]),
	    loop(State)
    end.

room_message(#room{players=P,messages=M}) ->
    Message = lists:nth(random:uniform(length(M)),M),
    [ Player ! {roommsg,Message} || Player <- P ].
