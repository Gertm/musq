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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% helper function
-export([buddy_process/1]).

-record(room, {name,
	       exits=[],
	       desc=[],
	       npcs=[],
	       objects=[],
	       players=[],
	       messages=[]
	      }).

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    spawn(?MODULE,buddy_process,[self()]),
    {ok, #room{}}.

buddy_process(Pid) ->
    receive
	{exit,Reason} -> exit(Reason)
    after random:uniform(15000) ->
	    Pid ! {tick},
	    buddy_process(Pid)
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
handle_call({load,Name},_From,State) ->
    %% load the room information from disk
    %% or load the room information from the area defenition.
    %% register yourself in the mnesia database
    NewState = State#room{name=Name},
    {reply,{ok,Name},NewState};

handle_call({add_exit,Name,Roomspec},_From,State) ->
    Exits = State#room.exits,
    NewState = State#room{exits=[{Name,Roomspec}|Exits]},
    {reply,ok,NewState};

handle_call({init,RoomFileLoc},_From,State) ->
    [RoomSpec] = file:consult(RoomFileLoc),
    {Name,Exits,Desc,Npc,Obj,Players} = RoomSpec,
    NewState = State#room{name=Name,exits=Exits,desc=Desc,npcs=Npc,objects=Obj,players=Players},
    {reply,ok,NewState};

handle_call({show_exits},_From,State) ->
    {reply,{exits, State#room.exits},State};

handle_call({enter,SourceDirection},_From,State) ->
    %% add the player to the state, send player the 'look' information.
    {reply,ok,State};

handle_call({look},_From,State) ->
    %% build up the lines for the room description.
    %% best to do this in a seperate function because we're going to need it elsewhere too.
    {reply,{look,[]},State};

handle_call({move,Direction},_From,State) ->
    {reply,ok,State};

handle_call({tick},_From,#room{players=P,messages=M}) ->  %% for room messages
    case length(P) of
	0 -> ok; %% why strain the system if there are no players listening anyway?
	_ -> Message = lists:nth(random:uniform(length(M)),M),
	     io:format("RoomMsg: ~s -> ~s~n.",[pid_to_list(self()),Message]),
	     [ Player ! {roommsg,Message} || Player <- P ]
    end;

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
%%% Internal functions
%%%===================================================================

%% need stuff to handle entrance and leaving of rooms.

%% gen_server:call(Pid, {enter,Direction}). will do the job
%% this will return a value like any other function would.
