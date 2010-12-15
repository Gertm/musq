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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(tile, {x				::integer(), 
			   y				::integer(), 
			   images			::[string()], 
			   properties		::[term()]
			  }).

-record(area, {name				::string(), 
			   width			::integer(), 
			   height			::integer(), 
			   defaulttile		::#tile{}, 
			   bordertile		::#tile{}, 
			   tiles			::[#tile{}], 
			   playerpids		::[pid()], 
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
start_link() ->
	gen_server:start_link(?MODULE, [], []).

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
init(AreaFilename) ->
	AreaState = parse_json(load(AreaFilename)), 
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
%%% Internal functions
%%%===================================================================

%% for the embedded starting of the yaws server (we'll need this later)
%% Yconf = [{docroot, "/home/gert/src/musq/client/"}, {port, 8080}, {listen, {127, 0, 0, 1}}, {appmods, [{"/service", wshandle}, {"/js/musq-config.js", musqconfig}]}].


%% parsing the JSON:
-spec(load(FileName::string()) -> term()).
%% @spec -spec(load(FileName::string()) -> term()
load(FileName) ->
    mochijson:decode(readlines(FileName)).
	
-spec(readlines(FileName::string()) -> string()).
%% @spec readlines(FileName::string()) -> string()
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]), 
    lists:flatten(get_all_lines(Device, [])).

%% could optimize this further to use binaries.
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [Line|Accum])
    end.

broadcast(Message, PlayerPids) ->
	[ Pid ! Message || Pid <- PlayerPids ].


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
	#area{name=Name, 
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

%% @doc
-spec(client_area_definition(#area{}) -> string()).
client_area_definition(#area{name=Name, 
							 width=Width, 
							 height=Height, 
							 defaulttile=DefaultTile, 
							 bordertile=BorderTile, 
							 tiles=Tiles}) ->
	mochijson:encode({struct, [{"Name", Name}, 
							   {"Width", Width}, 
							   {"Height", Height}, 
							   {"DefaultTile", client_tile_definition(DefaultTile)}, 
							   {"BorderTile", client_tile_definition(BorderTile)}, 
							   {"Tiles", {array, [ client_tile_definition(T) || T <- Tiles ]}}]}).

%% for future reference on using Eunit. (it's been a while..)
dummy_adder(X, Y) ->
	X + Y.

dummy_adder_test() ->
	?assertEqual(dummy_adder(4, 5), 9), 
	?assertEqual(dummy_adder(2, 3), 5).
