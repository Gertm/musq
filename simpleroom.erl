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
	       desc=["The first desc of the room.","second line in the desc."],
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
	    NewState = add_player_to_room(PlayerPid,State),
	    PlayerPid ! {ok,State#room.name},
	    io:format("~s entered room ~s.~n",[pid_to_list(PlayerPid),pid_to_list(self())]),
	    loop(NewState);
	{stop} ->
	    io:format("~s~n",["Room killed!"]),
	    {ok,stopped};
	_ -> ok
    after 15000 ->
	    room_message(State),
	    loop(State)
    end.

room_message(#room{players=P,messages=M}) ->
    case length(P) of
	0 -> ok; %% why strain the system if there are no players listening anyway?
	_ -> Message = lists:nth(random:uniform(length(M)),M),
	     io:format("RoomMsg: ~s -> ~s~n.",[pid_to_list(self()),Message]),
	     [ Player ! {roommsg,Message} || Player <- P ]
    end.

add_player_to_room(PlayerPid,RoomState) ->
    Players = RoomState#room.players,
    NewPlayers = [PlayerPid|Players],
    RoomState#room{players=NewPlayers}.

send_room_desc_to_player(PlayerPid,RoomState) ->
    [ PlayerPid ! {print,X} || X <- RoomState#room.desc ].
