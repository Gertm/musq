%%%-------------------------------------------------------------------
%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2010 by Gert <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(player).

-behaviour(gen_server).
-include("musq.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

%% wrapper functions
-export([relay/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(WsPid) ->
	gen_server:start_link(?MODULE, [WsPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WsPid]) ->
	{ok, #plr{logged_in=false, wspid=WsPid}}.

%%--- HANDLE_CALL ---
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--- HANDLE_CAST ---
handle_cast({login, WsPid, Params}, State) ->
	erlang:display(Params),
	PlayerName = proplists:get_value("Username",Params),
	Password = proplists:get_value("Password",Params),
	gen_server:call(world,{login, PlayerName, Password}),
	{noreply, State#plr{wspid=WsPid}};
handle_cast({logout, _WsPid, _Params}, State) ->
	%% player:logout(
	{noreply, State#plr{logged_in = false}};
handle_cast({getFiles, From, Params},State) ->
	?InfoMsg("getFiles params: ~p~n",[Params]),
	case Params of
		[] -> ok;
		_ -> R = hlp:getFiles(Params),
			 From ! {reply, self(), R}
	end,
	{noreply, State};
handle_cast({createAccount, From, Params}, State) ->
	%% when the player is logged in, this shouldn't work.
	?InfoMsg("handling createaccount~n",[]),
	R = gen_server:call(world, {createAccount, Params}),
	From ! {reply, self(), R},
	{noreply, State};
handle_cast({keepalive, From, []},State) ->
	From ! {reply, self(), hlp:create_reply("keepalive",[])},
	{noreply, State};
handle_cast({relay, Reply}, State) ->
	State#plr.wspid ! {reply, self(), Reply},	
	{noreply, State};
handle_cast(Any, State) ->
	?InfoMsg("no idea what this is: ~p~n",[Any]),
	{noreply, State}.


%%--- HANDLE_INFO ---
handle_info(_Info, State) ->
	{noreply, State}.

%%--- TERMINATE & CODE_CHANGE ---
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Wrappers
%%%===================================================================

%% @doc
%% Relay will be used when other processes just want to send something
%% to the wshandle process of the player.
%% @end
relay(PlayerPid, Reply) ->
	gen_server:cast(PlayerPid, {relay, Reply}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

check_for_player(Player) when is_pid(Player) ->
	ok;
check_for_player(Player) ->
	ok.


