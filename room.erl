%%%-------------------------------------------------------------------
%%% @author Gert M <gert@packard>
%%% @copyright (C) 2010, Gert M
%%% @doc
%%% The module for rooms.
%%% @end
%%% Created :  9 May 2010 by Gert M <gert@packard>
%%%-------------------------------------------------------------------
-module(room).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
handle_info/2, code_change/3]).

%% helper function
-export([buddy_process/2, load/1, remove_player/2, add_player/2,
get_area_map/1, get_area_map_recursive/3, get_area_map_room_position/2, get_area_map_rooms_to_process/2,
broadcast_roomtalk_to_players/3]).

%% call/cast wrappers
-export([enter/2, get_exits/1, leave/2, look/1, go/2, save/1,
get_name/1, add_exit/3,talk/3]).

-include("records.hrl").

%% should we keep area information here, or will the area be the only
%% one who knows about the rooms. Do rooms need to know what area they
%% are in, in order to function correctly?

%%%===================================================================
%%% (gen_server) API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates the room
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(RoomFile) ->
    State = get_room_state_from_file(RoomFile), 
    gen_server:start_link({local, State#room.name}, ?MODULE, State, []).

load(RoomFile) ->  %% does the same, just types faster ;-)
    start_link(RoomFile).

get_room_state_from_file(RoomFile) ->
    {ok, [RoomSpec]} = file:consult(RoomFile), 
    {_Name, Exits, Desc, Npc, Obj, Players, Messages} = RoomSpec, 
    NewName = helpers:room_name_from_filename(RoomFile), 
    #room{name=NewName, exits=Exits, desc=Desc, npcs=Npc, objects=Obj, players=Players, messages=Messages}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init(RoomState) ->
    BuddyPid = spawn(tickbuddy, loop, [self(), 30000]), 
    io:format("~p started buddy process ~p~n", [self(), BuddyPid]), 
    {ok, RoomState}.

buddy_process(Pid, Timeout) ->
    receive
	{exit, Reason} -> exit(Reason);
	{timeout, NewTimeout} ->
	    buddy_process(Pid, NewTimeout)
    after Timeout ->
	    gen_server:cast(Pid, tick), 
	    buddy_process(Pid, Timeout)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({add_exit, Name, Roomspec}, _From, State) ->
    Exits = State#room.exits, 
    NewState = State#room{exits=[{Name, Roomspec}|Exits]}, 
    {reply, ok, NewState};

handle_call({reload, RoomFileLoc}, _From, State) ->
    {ok, [RoomSpec]} = file:consult(RoomFileLoc), 
    {Name, Exits, Desc, Npc, Obj, _Players, Messages} = RoomSpec, 
    NewState = State#room{name=Name, exits=Exits, desc=Desc, npcs=Npc, objects=Obj, messages=Messages}, 
    {reply, ok, NewState};

handle_call(get_exits, _From, State) ->
    {reply, {exits, State#room.exits}, State};

handle_call({enter, _SourceDirection}, {PlayerPid, _}, State) ->
    %% add the player to the state, send player the 'look' information.
    io:format("~p entered room ~p~n", [PlayerPid, self()]), 
    NewState = add_player(State, PlayerPid), 
    {reply, {ok, NewState#room.desc}, NewState};

handle_call({leave, _ToDirection}, _From, State) ->
    {reply, ok, State};

handle_call(look, _From, State) ->
    %% build up the lines for the room description.
    %% best to do this in a separate function because we're going to need it elsewhere too.
    Desc = State#room.desc, 
    AreaMap = get_area_map(State), 
    {reply, {looked, Desc, State#room.exits, AreaMap}, State};

handle_call({go, Direction}, From, State) ->
    %% optimize this later, because it's probably slow
    DirectionDict = dict:from_list(State#room.exits), 
    case dict:find(Direction, DirectionDict) of
	{ok, Pid} -> 
	    NewState = remove_player(State, From), 
	    enter(Pid, Direction), 
	    {reply, {new_room_pid, Pid}, NewState};
	error -> {reply, {error, "You cannot go that way."}, State}
    end;

handle_call({exit, Reason}, _From, _State) ->
    exit(Reason);

handle_call(save_to_db, _From, State) ->
    dbstuff:save_room(State), 
    {reply, ok, State};

handle_call(get_name, _From, State) ->
    {reply, State#room.name, State};

handle_call({talk_in_room,PlayerName,Message},_From,State) ->
    broadcast_roomtalk_to_players(State,PlayerName,Message),
    {reply,ok,State};

handle_call(_Request, _From, State) ->
    Reply = ok, 
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(tick, #room{players=P, messages=M} = State) ->
    case length(P) of
	0 -> ok; %% why strain the system if there are no players listening anyway?
	_ ->
	    case length(M) of
		0 -> ok; %% no message to send
		_ ->
		    Message = lists:nth(random:uniform(length(M)), M), 
		    %% io:format("RoomMsg: ~s -> ~s.~n", [pid_to_list(self()), Message]), 
		    lists:foreach(fun(Player) -> player:room_msg(Player, Message) end, P)
	    end
    end, 
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

%% Need to add code here to inform the exit rooms that this one is no 
%% longer available.
terminate(_Reason, #room{name=Name} = _State) ->
    io:format("Room '~s' is terminated.~n", [Name]), 
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% gen_server wrappers
%%%===================================================================

%% gen_server:call(Pid, {enter, Direction}). will do the job
%% this will return a value like any other function would.

enter(Room, FromDirection) ->
    gen_server:call(Room, {enter, FromDirection}).

leave(Room, ToDirection) ->
    gen_server:call(Room, {leave, ToDirection}).

get_exits(Room) ->
    gen_server:call(Room, get_exits).

add_exit(Room, Direction, Destination) ->
    gen_server:call(Room, {add_exit, Direction, Destination}).

look(Room) ->
    gen_server:call(Room, look).

go(Room, Direction) ->
    gen_server:call(Room, {go, Direction}).

save(Room) ->
    gen_server:call(Room, save_to_db).

get_name(Room) ->
    gen_server:call(Room, get_name).

talk(Room,PlayerName,Message) ->
    gen_server:call(Room,{talk_in_room,PlayerName,Message}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_player(State, PlayerPid) ->
    OldPlayers = State#room.players, 
    State#room{players=[PlayerPid|OldPlayers]}.
    
remove_player(State, PlayerPid) ->
    OldPlayers = State#room.players, 
    State#room{players=lists:delete(PlayerPid, OldPlayers)}.

get_area_map(State) ->
    Radius = ?AREAMAPRADIUS,
    InitialPosition = {0, 0, 0},
    RoomsToProcess = get_area_map_rooms_to_process(InitialPosition, State#room.exits),
    ProcessedRooms = [InitialPosition, State#room.name],
    [Position || {Position, _} <- get_area_map_recursive(Radius, RoomsToProcess, ProcessedRooms)].

get_area_map_recursive(Radius, RoomsToProcess, ProcessedRooms) ->
    case Radius =:= 0 of
	true ->
	    ProcessedRooms;
	false ->
	    ProcessRoom = fun({Position, Pid}) ->
				  case (lists:member(Pid, ProcessedRooms)) of
				      true ->
					  [];
				      false ->
					  {exits, Exits} = get_exits(Pid),
					  get_area_map_rooms_to_process(Position, Exits)
				  end
			  end,
	    NewRoomsToProcess = lists:flatten(lists:map(ProcessRoom, RoomsToProcess)),
	    NewProcessedRooms = RoomsToProcess++ProcessedRooms,
	    get_area_map_recursive(Radius - 1, NewRoomsToProcess, NewProcessedRooms)
    end.

get_area_map_room_position({X, Y, Z}, Dir) ->
    case Dir of
	"north" -> {X, Y + 1, Z};
	"south" -> {X, Y - 1, Z};
	"west" -> {X - 1, Y, Z};
	"east" -> {X + 1, Y, Z};
	"northwest" -> {X - 1, Y + 1, Z};
	"northeast" -> {X + 1, Y + 1, Z};
	"southeast" -> {X + 1, Y - 1, Z};
	"southwest" -> {X - 1, Y - 1, Z};
	"up" -> {X, Y, Z + 1};
	"down" -> {X, Y, Z - 1}
    end.

get_area_map_rooms_to_process(Position, Exits) ->
    [{get_area_map_room_position(Position, Dir), Pid} || {Dir, Pid} <- Exits].

broadcast_roomtalk_to_players(State, PlayerName, Message) ->
    Players = State#room.players,
    lists:foreach(fun(Pid) ->
			  player:room_talk(Pid, PlayerName, Message) end,
		  Players).
