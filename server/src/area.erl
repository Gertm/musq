
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
%% Starts the server
%%--------------------------------------------------------------------
start_link(AreaFileName) ->
	gen_server:start_link(?MODULE, [AreaFileName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Initializes the server
%%--------------------------------------------------------------------
init([AreaFilename]) ->
	?show("Area ~s starting up~n",[AreaFilename]),
	AreaState = get_area_state(AreaFilename),
	{ok, AreaState}.

get_area_state(AreaFilename) ->
	parse_json(hlp:load_json(AreaFilename)).

%%--------------------------------------------------------------------
%% Handling call messages
%%--------------------------------------------------------------------
handle_call({player_enter, PlayerPid, PlayerName}, _From, State) ->
	%% send the area definition file
	?show("Doing handle_call({player_enter, ~p, ~p}, _From, State)~n",
			  [PlayerPid, PlayerName]),
	player:change_area(PlayerPid, State#area.name),
	Adef = client_area_definition(State),
	player:relay(PlayerPid, Adef),
	[ player:relay(PlayerPid, E) || E <- enter_req_of_all(State) ],
	%% update player db to reflect being in this area
	NewState = add_player(PlayerName, PlayerPid, State),
	EnterRequest = enter_request(PlayerName, NewState),
	broadcast_to_players(EnterRequest, NewState),
	%% check the entrance if there is nobody there, if there is,
	%% take an adjacent tile and put the player there.
	{reply, ok, NewState};
handle_call({player_leave, PlayerPid, PlayerName}, _From, State) ->
	%% PlayerPid and _From are usually going to be the same. 
	%% maybe clean this up so it just uses 'From'?
	NewState = remove_player(PlayerName, PlayerPid, State),
	?show("Removing ~s from ~p~n",[PlayerName, State#area.name]),
	%% send vanish request to clients
	broadcast_to_players(vanish_request(PlayerName, NewState), NewState),
	{reply, ok, NewState};
handle_call({player_move, _PlayerPid, PlayerName, X, Y}, _From, #area{tiles=Tiles}=AreaState) ->
	PlayerPos = get_player_pos(PlayerName, Tiles),
	NewPosition = best_tile_from_to(PlayerPos, {X, Y}, AreaState),
	NewTiles = set_player_pos(PlayerName, NewPosition, AreaState),
	NewState = AreaState#area{tiles=NewTiles},
	MoveReq = move_request(PlayerName, NewPosition, AreaState),
	broadcast_to_players(MoveReq, NewState),
	{reply, ok, NewState};
handle_call(chatHistory, _From, State) ->
	{reply, State#area.chatlines, State};
handle_call({get_player_pos, PlayerName}, _From, State) ->
	{X, Y} = get_player_pos(PlayerName, State#area.tiles),
	{reply, {X, Y}, State};
handle_call(_Request, _From, State) ->
	?show("AREA CALL UNKNOWN: ~p~n",[_Request]),
	Reply = ok, 
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Handling cast messages
%%--------------------------------------------------------------------
handle_cast({talk, PlayerName, Message}, State) ->
	ChatLine = {PlayerName, Message},
	%% ?show("In handle_cast talk: ~p~n",[ChatLine]),
	broadcast_to_players(
	  hlp:create_reply("talk",
					   [{"Name",PlayerName},
						{"Message",Message}]),State),
	{noreply, State#area{chatlines=[ChatLine | State#area.chatlines]}};
handle_cast(_Msg, State) ->
	?show("WTF IS THIS AREAMSG? -> ~p~n",[_Msg]),
	{noreply, State}.

handle_info(Info, State) ->
	?show("UNEXPECTED INFO MSG: ~p~n",[Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

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
	ReplyLines = lists:reverse([ {struct, [{"From", From}, {"Msg", Msg}]} 
								 || {From, Msg} <- Lines ]),
	%% io:format("Area ~p Chat History Lines: ~p~n",[AreaPid, ReplyLines]),
	WsPid ! {reply, 
			 self(), 
			 hlp:create_reply("chatHistory", [{"Lines", {array, ReplyLines}}])}.

player_move(Area, PlayerPid, PlayerName, X, Y) ->
	AreaPid = pid_of(Area),
	gen_server:call(AreaPid, {player_move, PlayerPid, PlayerName, X, Y}).

player_pos(Area, PlayerName) ->
	AreaPid = pid_of(Area),
	gen_server:call(AreaPid, {get_player_pos, PlayerName}).

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
	DefTileRecord = get_tile_def(DefaultTile),
	#area{name=list_to_atom(string:to_lower(Name)), 
		  width=Width, 
		  height=Height, 
		  defaulttile=DefTileRecord, 
		  bordertile=get_tile_def(BorderTile), 
		  tiledefs=T, 
		  playerpids=[], 
		  chatlines=[],
		  tiles=make_tile_dict_from_tile_defs(T, DefTileRecord),
		  world = nil}.

get_tile_def({struct, [{"Images", Images}, {"Properties", {struct, Properties}}]}) ->
	#tiledef{x=0, y=0, images=Images, properties=Properties};
get_tile_def({struct, [{"X", X}, {"Y", Y}, {"Images", Images}, {"Properties", {struct, Properties}}]}) ->
	#tiledef{x=X, y=Y, images=Images, properties=Properties}.

-spec(client_tile_definition(#tiledef{}) -> term()).									
client_tile_definition(#tiledef{x=X, y=Y, images=Images, properties=Properties}) ->
	{struct, [{"X", X}, {"Y", Y}, {"Images", Images}, {"Properties", {struct, Properties}}]}.

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
			Tiles = State#area.tiles,
			%% this will need to be reworked so it looks for the closest empty tile.
			NewTiles = dict:store({0, 0}, #tileprops{player=PlayerName}, Tiles),
			State#area{playerpids=[{PlayerName, PlayerPid} | PP], tiles=NewTiles}
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

jump_request(PlayerName, #area{tiles=TileProps}=Area) ->
	{X,Y} = get_player_pos(PlayerName, TileProps),
	jump_request(PlayerName, {X,Y}, Area).

move_request(PlayerName, {X, Y}, #area{name=AreaName}) ->
	hlp:create_reply("move",
					 [{"Name", PlayerName},
					  {"Area", atom_to_list(AreaName)},
					  {"X", X},
					  {"Y", Y}]).

chatline(Name, Message) ->
	{H, M, S} = erlang:time(),
	io_lib:format("~s:~s:~s <~s> ~s",[H,M,S,Name,Message]).

vanish_request(PlayerName, #area{name=AreaName}) ->
	hlp:create_reply("vanish",[{"Name", PlayerName}, {"Area", AreaName}]).

enter_req_of_all(#area{playerpids=PlayerPids}=Area) ->
	%% ?("Playerpids: ~p~n",[PlayerPids]),
	[ enter_request(PlayerName, Area) || {PlayerName, _PlayerPid} <- PlayerPids ].

enter_request(PlayerName, {X, Y}, #area{name=AreaName}=_Area) ->
	hlp:create_reply(
	  "enter",
	  [{"Name", PlayerName},
	   {"Area", AreaName},
	   {"X", X},
	   {"Y", Y},
	   {"Images", account:visual_part(PlayerName)}]).

enter_request(PlayerName, #area{tiles=TileProps}=Area) ->
	{X, Y} = get_player_pos(PlayerName, TileProps),
	enter_request(PlayerName, {X, Y}, Area).

%%----------------------------------------------
%% Pid helper functions
%%----------------------------------------------

pid_of(Area) when is_pid(Area) ->
	Area;
pid_of(Area) ->
	area_sup:get_area_pid(Area).

%%----------------------------------------------
%% tile helper functions
%%----------------------------------------------

-spec(make_tile_dict_from_tile_defs([#tiledef{}], #tiledef{}) -> dict()).
make_tile_dict_from_tile_defs(TileDefs, DefaultTile) ->
	L = lists:map(fun(#tiledef{x=X, y=Y, properties=P}) -> 
						  { {X,Y}, 
							#tileprops{
							  walkingspeed=get_walking_speed_from_proplist(P, DefaultTile)
							 } } end,
				  TileDefs),
	%%io:format("Making dict from list: ~p~n",[L]),
	dict:from_list(L).

get_walking_speed_from_proplist(Props,#tiledef{properties=DP}=_DefaultTile) ->
	case proplists:lookup("WalkingSpeed", Props) of
		none ->
			get_walking_speed_from_proplist(DP,#tiledef{});
		{"WalkingSpeed", SpeedStr} ->
			{S, _} = string:to_integer(SpeedStr),
			S
	end.

is_tile_available({X,Y}, #area{tiles=Tiles}=Area) ->
	{Av, Reason} = 
		case dict:find({X,Y}, Tiles) of
			error -> {true, #tileprops{}};
			{ok, Value} ->
				case Value#tileprops.player of
					undefined ->
						case Value#tileprops.walkingspeed of
							0 -> 
								%%io:format("Tile is unwalkable! {~p,~p}~n",[X,Y]),
								{false, unwalkable};
							_ -> {true, Value}
						end;
					_ -> {false, occupied}
				end
		end,
	B = is_tile_in_area({X, Y}, Area),
	%%io:format("is_tile_avail: (~p) and (~p) ~p~n",[Av, B, {Av and B, Reason}]),
	{Av and B, Reason}.

short_tile_avail({X,Y}, #area{}=Area) ->
	{TF, _} = is_tile_available({X,Y}, Area),
	TF.
	
is_tile_in_area({X,Y}, #area{width=Width, height=Height}) ->
	%%io:format("Width: ~p Height: ~p ~n",[Width, Height]),
	IsPositive = (X >= 0) and (Y >= 0),
	IsInside = (X < Width) and (Y < Height),
	%%io:format("is_tile_in_area: X: ~p Y: ~p~nW: ~p, H: ~p~nIsPositive (~p) and IsInside (~p)~n",
	%%		  [X, Y, Width, Height, IsPositive, IsInside]),
	IsPositive and IsInside.

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

set_player_pos(PlayerName, {X,Y}, #area{tiles=Tiles}=Area) ->
	{X1, Y1} = get_player_pos(PlayerName, Tiles),
	case is_tile_available({X,Y}, Area) of
		{false, _} ->
			Tiles;
		{true, Destination} ->
			Tiles2 = case {X1, Y1} of
						 {error, no_player} ->
							 Tiles;
						 {X2, Y2} ->
							 CurrentTileProps = dict:fetch({X1, Y1}, Tiles),
							 dict:store({X2 ,Y2}, CurrentTileProps#tileprops{player=undefined}, Tiles)
					 end,
			dict:store({X, Y}, Destination#tileprops{player=PlayerName},Tiles2)
	end.


available_neighbour_tiles({X, Y}, #area{}=AreaState) ->
	Adj = [ {X+A, Y+B} || 
				A <- lists:seq(-1,1),
				B <- lists:seq(-1,1),
				{X+A, Y+B} =/= {X,Y} ],
	lists:filter(
	  fun({I,J}) -> short_tile_avail({I,J}, AreaState) end,
	  Adj).

tile_distance({X1, Y1}, {X2, Y2}) ->
	abs(X2-X1) + abs(Y2-Y1).

select_lowest_scoring_tile(CoordList, {Xdest, Ydest}) ->
	lists:foldl(
	  fun({X1, Y1}, {X2, Y2}) ->
			  Dist1 = tile_distance({X1, Y1}, {Xdest, Ydest}),
			  Dist2 = tile_distance({X2, Y2}, {Xdest, Ydest}),
			  if
				  Dist1 < Dist2 ->
					  {X1, Y1};
				  true ->
					  {X2, Y2}
			  end
	  end,
	  {9999,9999},
	  CoordList).

best_tile_from_to({Xorigin, Yorigin}, {X, Y}, #area{}=AreaState) ->
	AdjTiles = available_neighbour_tiles({Xorigin, Yorigin}, AreaState),
	select_lowest_scoring_tile(AdjTiles, {X, Y}).

