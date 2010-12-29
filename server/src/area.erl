%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Area generic server
%%% @end
%%% Created : 12 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(area).
-compile(export_all).
-include("musq.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).

-record(tile, {x				::integer(), 
			   y				::integer(), 
			   images			::[string()], 
			   properties		::[term()]
			  }).

-record(area, {name				::term(), 
			   width			::integer(), 
			   height			::integer(), 
			   defaulttile		::#tile{}, 
			   bordertile		::#tile{}, 
			   tiles			::[#tile{}], 
			   playerpids		::[tuple()], 
			   world			::pid()
			  }).


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
start_link(AreaFileName) ->
	gen_server:start_link(?MODULE, [AreaFileName], []).

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
init([AreaFilename]) ->
	?InfoMsg("Area ~s starting up~n",[AreaFilename]),
	AreaState = parse_json(hlp:load_json(AreaFilename)), 
	{ok, AreaState}.


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
handle_call({player_enter, PlayerPid, PlayerName}, _From, State) ->
	%% update player db to reflect being in this area
	NewState = add_player(PlayerName, PlayerPid, State),
	%% send the area definition file
	Adef = client_area_definition(State),
	player:relay(PlayerPid, Adef),
	%% check the entrance if there is nobody there, if there is,
	%% take an adjacent tile and put the player there.
	Jr = jump_request(PlayerName, {0, 0}, State),
	player:relay(PlayerPid, Jr),
	%% still need to broadcast to the other players.
	{reply, ok, NewState};
handle_call({player_leave, PlayerPid, PlayerName}, _From, State) ->
	NewState = remove_player(PlayerName, PlayerPid, State),
	%% send vanish request to clients
	{reply, ok, NewState};
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
%%% Wrappers
%%%===================================================================

player_enter(AreaPid, PlayerPid, PlayerName) ->
	gen_server:call(AreaPid, {player_enter, PlayerPid, PlayerName}).

player_leave(AreaPid, PlayerPid, PlayerName) ->
	gen_server:call(AreaPid, {player_leave, PlayerPid, PlayerName}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

broadcast_to_players(Message, #area{playerpids=PlayerPids}) ->
	[ player:relay(Pid, Message) || {_, Pid} <- PlayerPids ].


%% downside of this is the area definition files will need to be in the correct order
%% but since we're going to generate them later on, that shouldn't be a problem.
parse_json(Json) ->
	{struct, [{"Name", Name}, 
			 {"Width", Width}, 
			 {"Height", Height}, 
			 {"DefaultTile", DefaultTile}, 
			 {"BorderTile", BorderTile}, 
			 {"Tiles", {array, Tiles}}]} = Json, 
	T = [ get_tile(Tile) || Tile <- Tiles ], 
	#area{name=list_to_atom(Name), 
		  width=Width, 
		  height=Height, 
		  defaulttile=get_tile(DefaultTile), 
		  bordertile=get_tile(BorderTile), 
		  tiles=T, 
		  playerpids=[], 
		  world = nil}.

get_tile({struct, [{"Images", Images}, {"Properties", Properties}]}) ->
	#tile{x=0, y=0, images=Images, properties=Properties};
get_tile({struct, [{"X", X}, {"Y", Y}, {"Images", Images}, {"Properties", Properties}]}) ->
	#tile{x=X, y=Y, images=Images, properties=Properties}.

-spec(client_tile_definition(#tile{}) -> term()).									
client_tile_definition(#tile{x=X, y=Y, images=Images, properties=Properties}) ->
	{struct, [{"X", X}, {"Y", Y}, {"Images", Images}, {"Properties", Properties}]}.

-spec(client_area_definition(#area{}) -> string()).
client_area_definition(#area{name=Name, 
							 width=Width, 
							 height=Height, 
							 defaulttile=DefaultTile, 
							 bordertile=BorderTile, 
							 tiles=Tiles}) ->
	{struct, [{"Name", atom_to_list(Name)}, 
			  {"Width", Width}, 
			  {"Height", Height}, 
			  {"DefaultTile", client_tile_definition(DefaultTile)}, 
			  {"BorderTile", client_tile_definition(BorderTile)}, 
			  {"Tiles", {array, [ client_tile_definition(T) || T <- Tiles ]}}]}.

add_player(PlayerName, PlayerPid, State) ->
	case proplists:is_defined(PlayerName, State#area.playerpids) of
		true ->
			State;
		false ->
			PP = State#area.playerpids,
			State#area{playerpids=[{PlayerName, PlayerPid} | PP]}
	end.

remove_player(PlayerName, _PlayerPid, State) ->
	case proplists:is_defined(PlayerName, State#area.playerpids) of
		true ->
			PP = proplists:delete(PlayerName,State#area.playerpids),
			State#area{playerpids=PP};
		false ->
			State
	end.

%% jump_request should really get the last known location of the player
%% from the database, not just take X and Y as parameters.
%% or get the destination from the area spec when spawning in a new area.
jump_request(PlayerName, {X,Y}, #area{name=AreaName}=_Area) ->
	hlp:create_reply("jump",[{"X",X},{"Y",Y},{"Name",PlayerName},{"Area",AreaName}]).

