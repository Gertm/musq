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
-include("records.hrl").
-define(DEFAULTROOM, simpleroom).

listener() ->
    register(listener, spawn(?MODULE, getmsg,[])).

getmsg() ->
    receive
	stop -> exit("stopping");
	X -> io:format("listener: ~p~n",[X])
    after 1000 ->
	    nothing_received
    end,
    getmsg().

newroom() ->
    Room = #room{name="testroom1", desc=["The first desc of the room.","second line in the desc."], messages=["room msg 1","room msg 2"]},
    spawn(?MODULE, loop, [Room]).

set_default_room(RoomPid) ->
    register(?DEFAULTROOM, RoomPid).

get_default_room() ->
    ?DEFAULTROOM.

loop(RoomState) ->
    receive
	{init, RoomFileLoc} ->
	    {ok, [RoomSpec]} = file:consult(RoomFileLoc),
	    {Name, Exits, Desc, Npc, Obj, Players} = RoomSpec,
	    NewRoomState = RoomState#room{name=Name, exits=Exits, desc=Desc, npcs=Npc, objects=Obj, players=Players},
	    loop(NewRoomState);
	{stop} ->
	    io:format("~s~n", ["Room killed!"]),
	    {ok, stopped};
	{enter, PlayerPid} ->
	    NewRoomState = enter(PlayerPid, RoomState),
	    loop(NewRoomState);
	{look, PlayerPid} ->
	    look(PlayerPid, RoomState),
	    loop(RoomState);
	_ -> ok
    after 15000 ->
	    io:format("~s~n",["tick of "++pid_to_list(self())]),
	    room_message(RoomState),
	    loop(RoomState)
    end.

room_message(#room{players=P, messages=M}) ->
    case length(P) of
	0 -> ok; %% why strain the system if there are no players listening anyway?
	_ -> Message = lists:nth(random:uniform(length(M)), M),
	     io:format("RoomMsg: ~s -> ~s.~n", [pid_to_list(self()), Message]),
	     [ Player ! {print, Message} || Player <- P ]
    end.

enter(PlayerPid, RoomState) ->
    Players = RoomState#room.players,
    NewPlayers = [PlayerPid|Players],
    NewRoomState = RoomState#room{players=NewPlayers},
    PlayerPid ! {room_entered, self(), RoomState#room.name},
    io:format("~s entered room ~s.~n", [pid_to_list(PlayerPid), pid_to_list(self())]),
    NewRoomState.

look(PlayerPid, RoomState) ->
    [ PlayerPid ! {print, X} || X <- RoomState#room.desc ].
