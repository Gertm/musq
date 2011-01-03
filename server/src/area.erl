
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%% Area generic server
%%% @end
%%% Created : 12 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(area).
-compile([export_all, debug_info]).
-include("musq.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).

-record(tiledef, {x                 ::integer(), 
				  y                 ::integer(), 
				  images	        ::[string()], 
				  properties		::[term()]
				 }).

-record(tileprops, {player  ::string(),
					walkingspeed=1 ::integer()
				   }).

-record(area, {name				::term(), 
			   width			::integer(), 
			   height			::integer(), 
			   defaulttile		::#tiledef{}, 
			   bordertile		::#tiledef{}, 
			   tiledefs			::[#tiledef{}], 
			   playerpids		::[tuple()], 
			   world			::pid(),
			   chatlines        ::[tuple()],
			   tiles            ::dict()
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
	%% send the area definition file
	player:change_area(PlayerPid, State#area.name),
	Adef = client_area_definition(State),
	player:relay(PlayerPid, Adef),
	[ player:relay(PlayerPid, V) || V <- visual_of_all(State#area.playerpids) ],
	[ player:relay(PlayerPid, J) || J <- jump_req_of_all(State) ],
	%% update player db to reflect being in this area
	NewState = add_player(PlayerName, PlayerPid, State),
	VisualRequest = player:get_visual_request(PlayerPid),
	broadcast_to_players(VisualRequest, NewState),
	
	%% this client specifically needs the visual requests and
	%% jump requests of the other clients.
	
	%% check the entrance if there is nobody there, if there is,
	%% take an adjacent tile and put the player there.
	JumpRequest = jump_request(PlayerName, {0, 0}, NewState),
	broadcast_to_players(JumpRequest, NewState),
	{reply, ok, NewState};
handle_call({player_leave, PlayerPid, PlayerName}, _From, State) ->
	%% PlayerPid and _From are usually going to be the same. 
	%% maybe clean this up so it just uses 'From'?
	NewState = remove_player(PlayerName, PlayerPid, State),
	io:format("Removing ~s from ~p~n",[PlayerName, State#area.name]),
	%% send vanish request to clients
	{reply, ok, NewState};
handle_call(chatHistory, _From, State) ->
	{reply, State#area.chatlines, State};
handle_call(_Request, _From, State) ->
	io:format("AREA CALL UNKNOWN: ~p~n",[_Request]),
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
handle_cast({talk, PlayerName, Message}, State) ->
	ChatLine = {PlayerName, Message},
	io:format("In handle_cast talk: ~p~n",[ChatLine]),
	broadcast_to_players(
	  hlp:create_reply("talk",
					   [{"Name",PlayerName},
						{"Message",Message}]),State),
	{noreply, State#area{chatlines=[ChatLine | State#area.chatlines]}};
handle_cast(_Msg, State) ->
	io:format("WTF IS THIS AREAMSG? -> ~p~n",[_Msg]),
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

player_enter(Area, PlayerPid, PlayerName) ->
	AreaPid = pid_of(Area),
	gen_server:call(AreaPid, {player_enter, PlayerPid, PlayerName}).

player_leave(Area, PlayerPid, PlayerName) ->
	AreaPid = pid_of(Area),
	gen_server:call(AreaPid, {player_leave, PlayerPid, PlayerName}).

talk(Area, PlayerName, Message) ->
	AreaPid = pid_of(Area),
	%%io:format("In area:talk P:~s, M:~s~n",[PlayerName, Message]),
	gen_server:cast(AreaPid, {talk, PlayerName, Message}).

chat_history(Area, WsPid) ->
	AreaPid = pid_of(Area),
	Lines = gen_server:call(AreaPid, chatHistory),
	ReplyLines = [ {struct, [{"From", From}, {"Msg", Msg}]} || {From, Msg} <- Lines ],
	io:format("Area ~p Chat History Lines: ~p~n",[AreaPid, ReplyLines]),
	WsPid ! {reply, 
			 self(), 
			 hlp:create_reply("chatHistory", [{"Lines", {array, ReplyLines}}])}.


%%%===================================================================
%%% internal functions
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
			 {"Tiles", {array, Tiledefs}}]} = Json, 
	T = [ get_tile_def(Tile) || Tile <- Tiledefs ], 
	#area{name=list_to_atom(string:to_lower(Name)), 
		  width=Width, 
		  height=Height, 
		  defaulttile=get_tile_def(DefaultTile), 
		  bordertile=get_tile_def(BorderTile), 
		  tiledefs=T, 
		  playerpids=[], 
		  chatlines=[],
		  tiles=make_tile_dict_from_tile_defs(T),
		  world = nil}.

get_tile_def({struct, [{"Images", Images}, {"Properties", Properties}]}) ->
	#tiledef{x=0, y=0, images=Images, properties=Properties};
get_tile_def({struct, [{"X", X}, {"Y", Y}, {"Images", Images}, {"Properties", Properties}]}) ->
	#tiledef{x=X, y=Y, images=Images, properties=Properties}.

-spec(client_tile_definition(#tiledef{}) -> term()).									
client_tile_definition(#tiledef{x=X, y=Y, images=Images, properties=Properties}) ->
	{struct, [{"X", X}, {"Y", Y}, {"Images", Images}, {"Properties", Properties}]}.

-spec(client_area_definition(#area{}) -> string()).
client_area_definition(#area{name=Name, 
							 width=Width, 
							 height=Height, 
							 defaulttile=DefaultTile, 
							 bordertile=BorderTile, 
							 tiledefs=Tiledefs}) ->
	hlp:create_reply(
	  "area", 
	  [{"Name", atom_to_list(Name)}, 
	   {"Width", Width}, 
	   {"Height", Height}, 
	   {"DefaultTile", client_tile_definition(DefaultTile)}, 
	   {"BorderTile", client_tile_definition(BorderTile)}, 
	   {"Tiles", {array, [ client_tile_definition(T) || T <- Tiledefs ]}}]).

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

jump_req_of_all(#area{playerpids=PlayerPids}=State) ->
	[ jump_request(Name, {0,0}, State) || {Name, _Pid} <- PlayerPids ].

visual_of_all(PlayerPids) ->
	[ player:get_visual_request(Pid) || {_Name, Pid} <- PlayerPids ].

chatline(Name, Message) ->
	{H, M, S} = erlang:time(),
	io_lib:format("~s:~s:~s <~s> ~s",[H,M,S,Name,Message]).

pid_of(Area) when is_pid(Area) ->
	Area;
pid_of(Area) ->
	world:get_area_pid(Area).

%% tile helper functions

-spec(make_tile_dict_from_tile_defs([#tiledef{}]) -> dict()).
make_tile_dict_from_tile_defs(TileDefs) ->
	L = lists:map(fun(#tiledef{x=X, y=Y}) -> { {X,Y}, #tileprops{} } end,
				  TileDefs),
	dict:from_list(L).

is_tile_available({X,Y}, Tiles) ->
	case dict:find({X,Y}, Tiles) of
		error -> {true, #tileprops{}};
		{ok, Value} ->
			case Value#tileprops.player of
				undefined ->
					case Value#tileprops.walkingspeed of
						0 -> {false, unwalkable};
						_ -> {true, Value}
					end;
				_ -> {false, occupied}
			end
	end.		

%% this desperately needs unit tests!
-spec(get_player_pos(string(), dict()) -> tuple()).
get_player_pos(PlayerName, Tiles) ->
	PlayerTiles = dict:to_list(
					dict:filter(fun(_Key, Value) ->
										PlayerName == Value#tileprops.player end,
								Tiles)),
	case PlayerTiles of
		[] -> {error, no_player};
		_ -> {{X,Y}, _} = hd(PlayerTiles),
			 {X,Y}
	end.

set_player_pos(PlayerName, {X,Y}, Tiles) ->
	{X1, Y1} = get_player_pos(PlayerName, Tiles),
	CurrentTileProps = dict:fetch({X1, Y1}, Tiles),
	case is_tile_available({X,Y}, Tiles) of
		{false, _} ->
			Tiles;
		{true, Destination} ->
			Tiles2 = case {X1, Y1} of
						 {error, no_player} ->
							 Tiles;
						 {X2, Y2} ->
							 dict:store({X2 ,Y2}, CurrentTileProps#tileprops{player=undefined}, Tiles)
					 end,
			NewTiles = dict:store({X, Y}, Destination#tileprops{player=PlayerName},Tiles2),
			NewTiles
	end.
