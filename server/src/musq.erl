%%%-------------------------------------------------------------------
%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2010 by Gert <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(musq).
-include("musq.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	case musq_sup:start_link() of
		{ok, Pid} ->
			application:start(mnesia),
			start_yaws(),
			{ok, Pid};
		Error ->
			Error
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_yaws() ->
	yaws:start_embedded("/home/gert/src/musq/client",
                       [{servername, "musqweb"}, 
						{port, 8080},
						{appmods,[{"/service",wshandle},
								  {"/js/musq-config.js",musqconfig}]},
						{listen, {0,0,0,0}}],
                       [{auth_log, false},
						{logdir, "/home/gert/logs"},
						{copy_errlog, false}]).
