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
start_link(WsPid) ->
	gen_server:start_link(?MODULE, [WsPid], []).

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
init([WsPid]) ->
	{ok, #plr{logged_in=false, wspid=WsPid}}.

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
handle_cast({login, WsPid, Params}, State) ->
	WsPid ! {reply, self(), Params},
	{noreply, State#plr{wspid=WsPid}};
handle_cast({getFiles, From, Params},State) ->
	?InfoMsg("getFiles params: ~p~n",[Params]),
	case Params of
		[] -> ok;
		_ -> R = hlp:getFiles(Params),
			 ?InfoMsg("Sending back to wshandle: ~p~n",[R]),
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
	From ! {reply, self(), hlp:createReply("keepalive",[])},
	{noreply, State};
handle_cast(Any, State) ->
	?InfoMsg("no idea what this is: ~p~n",[Any]),
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

read_player(PlayerName) ->
	mnesia:transaction(fun() -> mnesia:read(player, PlayerName) end).

dirty_read_player(PlayerName) ->
	ok.

is_logged_in(PlayerName) ->
	Loggedin = read_player(PlayerName),
	case Loggedin of
		{atomic, []} ->
			no_user;
		{atomic, #plr{}=P} ->
			P#plr.logged_in
	end.

user_login(PlayerName, Password) ->
	case is_logged_in(PlayerName) of
		no_user ->
			{error, "no such user"};
		true ->
			{error, "already logged in"};
		false ->
			A = account:read_account(PlayerName),
			if
				A#account.password == Password ->
					
					ok;
				true ->
					{error, "incorrect password"}
			end
	end.

log_in_user(PlayerName,TrueFalse) ->
	{atomic, #plr{}=Player} = read_player(PlayerName),
	mnesia:transaction(fun() -> mnesia:write(player, Player#plr{logged_in = TrueFalse}) end).


get_visual(PlayerName) ->
	{atomic, #plr{}=P} = read_player(PlayerName).
	
