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
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(AREAPATH, "../../server/areas/").

-record(arearec, {name ::string(),
				  pid  ::pid()}).
-record(worldstate, {areas ::[#arearec{}],
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
	{ok, #worldstate{}}.

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
handle_call({add_player, PlayerName},WsHandlePid, State) ->
	NewDict = dict:append(PlayerName,WsHandlePid,State#worldstate.players),
	State#worldstate{players=NewDict};
handle_call({login, PlayerName, Password}, WsHandlePid, State) ->
	%% need to get into mnesia here and check if the player exists.
	{reply, ok, State};
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


%% get the names of all the areas in the area folder
-spec(get_area_filenames() -> [string()]).
get_area_filenames() ->
	{ok, FileList} = file:list_dir(?AREAPATH),
	lists:filter(fun is_area_filename/1, FileList).

-spec(is_area_filename(FileName ::string()) -> boolean()).
is_area_filename(FileName) ->
	ExtStart = string:len(FileName) - 4,
	Ext = string:substr(FileName, ExtStart),
	if Ext == ".area" ->
			true;
	   true -> false
	end.

