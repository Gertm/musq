%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert Meulyzer
%%% @doc
%%%
%%% @end
%%% Created : 27 Dec 2010 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(area_sup).
-include("musq.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Functions init needs
-export([area_child_specs/0, area_child_spec/1, get_filenames/0,
		 is_area_filename/1, area_name_from_filename/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	
	{ok, {SupFlags, area_child_specs()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

area_child_specs() ->
	AreaFileNames = area:get_filenames(),
    lists:map(fun area_child_spec/1, AreaFileNames).

area_child_spec(AreaFileName) ->
	AreaName = list_to_atom(area:area_name_from_filename(AreaFileName)),
	{AreaName,
	 {area, start_link, [AreaFileName]},
	 permanent,
	 2000,
	 worker,
	 [area]}.

-spec(get_filenames() -> [string()]).
get_filenames() ->
	{ok, FileList} = file:list_dir(?AREAPATH),
	FullPathList = lists:map(fun(X) -> ?AREAPATH ++ X end, FileList),
	lists:filter(fun is_area_filename/1, FullPathList).

-spec(is_area_filename(FileName ::string()) -> boolean()).
is_area_filename(FileName) ->
	ExtStart = string:len(FileName) - 4,
	Ext = string:substr(FileName, ExtStart),
	if Ext == ".area" ->
			true;
	   true -> false
	end.

area_name_from_filename(FileName) ->
	"area_" ++ filename:basename(FileName,".area").
