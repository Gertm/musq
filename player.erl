%%%-------------------------------------------------------------------
%%% @author  <Randy Voet <crimson13>>
%%% @copyright (C) 2010, 
%%% @doc
%%% Some general stuff should be parsed, nothing more.
%%% Basically this module only needs to accept 'print' events for stuff that needs to be
%%% written to the player's screen and it needs to accept the incoming stuff from the socket.
%%% From that, the module needs to decide where the message should be sent
%%% to the room, to another player, an NPC?,.. etc
%%% all other logic should be in other modules.
%%% @end
%%% Created :  2 Sep 2010 by  <Randy Voet <crimson13>>
%%%-------------------------------------------------------------------
-module(player).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% gen_server wrappers
-export([save/1, look/1, move/2]).

%% internal functions
-export([handle_notification/2]).

-include("telnetcolors.hrl").
-include("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link([Socket, Name]) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link([Name, ControllerPid]) ->
    State = #player{name=Name, controllerPid=ControllerPid},
    gen_server:start_link({local, list_to_atom("player_"++helpers:clean_name(Name))}, ?MODULE, State, []).

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
init(State) ->
    BuddyPid = spawn(tickbuddy, loop, [self(), 15000]),
    io:format("~p started buddy process ~p~n", [self(), BuddyPid]),
    {ok, State}.

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

%% temporary
handle_call(enter_default_room, _From, State) ->
    DefaultRoom = room_source1,
    room:enter(DefaultRoom, "somewhere"),
    NewState = State#player{room=DefaultRoom},
    handle_notification(NewState, "You've entered the default room."),
    {reply, ok, NewState};

handle_call(save, _From, State) ->
    R = dbstuff:save_player(State), %% > still need to write code for that
    {reply, R, State};

handle_call(look, _From, State) ->
    Room = State#player.room,
    Reply = room:look(Room),
    {reply, Reply, State};

handle_call({move, Direction}, _From, State) ->
    Room = State#player.room,
    Reply = room:move(Room, Direction),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

handle_cast(tick, State) ->
    {noreply, State};

handle_cast({roommsg, Message}, State) ->
    handle_notification(State, Message),
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
terminate(_Reason, _State) ->
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

save(Pid) ->
    gen_server:call(Pid, save).

move(Direction, Pid) ->
    gen_server:call(Pid, {move, Direction}).

look(Pid) ->
    gen_server:call(Pid, look).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_notification(#player{controllerPid=Pid}, Message) ->
    Pid ! {notification, Message}.
