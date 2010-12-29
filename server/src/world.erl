%%%-------------------------------------------------------------------
%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert
%%% @doc
%%% The world module handles the communication of the different areas.
%%% when a player leaves an area to go to another area, he/she has to
%%% pass through the world.
%%% @end
%%% Created : 13 Dec 2010 by Gert <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(world).

-behaviour(gen_server).
-compile(export_all).
-include("musq.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
%% this AREAPATH really needs to be changed.
%% maybe define some good PATH variable in musq.hrl

-record(worldstate, {areas ::[term()], 
					 players ::dict:dictonary()}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> 
	%% for this, a named version will do because there should only
	%% be one world. (for now)
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
	db:prepare_database(), 
	{ok, #worldstate{players=dict:new()}}.

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
handle_call({add_player, PlayerName}, From, State) ->
	NewDict = dict:append(PlayerName, From, State#worldstate.players), 
	NewState = State#worldstate{players=NewDict}, 
	{reply, ok, NewState};
handle_call({remove_player, PlayerName}, From, State) ->
	NewDict = dict:erase(PlayerName, State#worldstate.players), 
	{reply, ok, State#worldstate{players=NewDict}};
handle_call({spawn_player, WsPid}, _Pid, State) ->
	{ok, PlayerPid} = supervisor:start_child(musq_sup, player_child_spec(WsPid)), 
	{reply, PlayerPid, State};
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
handle_cast({createAccount, WsPid, Params}, State) ->
	Reply = account:create_account_nx(Params), 
	?InfoMsg("account creation reply: ~p~n", [Reply]), 
	WsPid ! {reply, self(), Reply}, 
	{noreply, State};
handle_cast({login, PlayerPid, Params}, State) ->
	PlayerName = proplists:get_value("Username", Params), 
	Password = proplists:get_value("Password", Params), 
	From = proplists:get_value("From", Params), 
	io:format("World handling login: Player: ~s, Pass: ~s, From: ~s~n",
			  [PlayerName, Password, From]),
	case login:login(PlayerName, Password, PlayerPid) of
		ok ->
			P = db:read_player(PlayerName),
			case P of
				[] -> ?InfoMsg("Woops, no player record!!",[]);
				_ ->
					{AreaName, {X, Y}} = case P#plr.area of
											 undefined ->
												 {'begin', {0, 0}};
											 Other ->
												 {Other, P#plr.position}
										 end,
					area:player_enter(get_area_pid(AreaName), PlayerPid, PlayerName)
			end;
			%% msg area player is entering.

		error ->
			ok
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

player_child_spec(WsPid) ->
	{list_to_atom("plr_" ++ pid_to_list(WsPid)), 
	 {player, start_link, [WsPid]}, 
	 temporary, 
	 2000, 
	 worker, 
	 ['player']}.
	 

get_area_pids() ->
	Children = supervisor:which_children(area_sup),
	io:format("which_children: ~p~n",[Children]),
	lists:map(fun({AreaName, Pid, _, _}) ->
					  {AreaName, Pid} end, 
			  Children).

get_area_pid(AreaName) ->
	proplists:get_value(AreaName, get_area_pids()).

