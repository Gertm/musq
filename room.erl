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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,load/1]).
%% helper function
-export([buddy_process/2]).

%% accessor functions
-export([exits/1,enter/2,leave/2]).

-include("records.hrl").
%% should we keep area information here, or will the area be the only
%% one who knows about the rooms. Do rooms need to know what area they
%% are in, in order to function correctly?

%%%===================================================================
%%% API
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
    gen_server:start_link({local,list_to_atom(State#room.name)},?MODULE, State, []).
    
load(RoomFile) ->  %% does the same, just types faster ;-)
    start_link(RoomFile).

get_room_state_from_file(RoomFile) ->
    {ok,[RoomSpec]} = file:consult(RoomFile),
    {Name,Exits,Desc,Npc,Obj,Players,Messages} = RoomSpec,
    #room{name=Name,exits=Exits,desc=Desc,npcs=Npc,objects=Obj,players=Players,messages=Messages}.

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
    BuddyPid = spawn(?MODULE,buddy_process,[self(),15000]),
    io:format("~s ~p~n",["Started buddy process:",BuddyPid]),
    {ok,RoomState}.

buddy_process(Pid,Timeout) ->
    receive
	{exit,Reason} -> exit(Reason);
	{timeout,NewTimeout} ->
	    buddy_process(Pid,NewTimeout)
    after Timeout ->
	    gen_server:cast(Pid,tick),
	    buddy_process(Pid,Timeout)
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

handle_call({add_exit,Name,Roomspec},_From,State) ->
    Exits = State#room.exits,
    NewState = State#room{exits=[{Name,Roomspec}|Exits]},
    {reply,ok,NewState};

handle_call({reload,RoomFileLoc},_From,State) ->
    OldPlayers = State#room.players,
    {ok,[RoomSpec]} = file:consult(RoomFileLoc),
    {Name,Exits,Desc,Npc,Obj,_Players,Messages} = RoomSpec,
    NewState = State#room{name=Name,exits=Exits,desc=Desc,npcs=Npc,objects=Obj,players=OldPlayers,messages=Messages},
    {reply,ok,NewState};

handle_call(show_exits,_From,State) ->
    {reply,{exits, State#room.exits},State};

handle_call({enter,_SourceDirection},{PlayerPid,_},State) ->
    %% add the player to the state, send player the 'look' information.
    io:format("~p~n",[PlayerPid]),
    OldPlayers = State#room.players,
    NewState = State#room{players=[PlayerPid|OldPlayers]},
    {reply,{ok,State#room.desc},NewState};

handle_call({leave,_ToDirection},_From,State) ->
    {reply,ok,State};

handle_call(look,_From,State) ->
    %% build up the lines for the room description.
    %% best to do this in a seperate function because we're going to need it elsewhere too.
    {reply,{look,[]},State};

handle_call({move,_Direction},_From,State) ->
    {reply,ok,State};

handle_call({exit,Reason},_From,_State) ->
    exit(Reason);

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
handle_cast(tick,#room{players=P,messages=M} = State) ->
    case length(P) of
	0 -> ok; %% why strain the system if there are no players listening anyway?
	_ -> Message = lists:nth(random:uniform(length(M)),M),
	     io:format("RoomMsg: ~s -> ~s~n.",[pid_to_list(self()),Message]),
	     [ Player ! {roommsg,Message} || Player <- P ]
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
    io:format("Room '~s' is terminated.~n",[Name]),
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
%%% Internal functions
%%%===================================================================

%% need stuff to handle entrance and leaving of rooms.

%% gen_server:call(Pid, {enter,Direction}). will do the job
%% this will return a value like any other function would.

enter(Room,FromDirection) ->
    gen_server:call(Room,{enter,FromDirection}).

leave(Room,ToDirection) ->
    gen_server:call(Room,{leave,ToDirection}).

exits(Room) ->
    gen_server:call(Room,show_exits).
