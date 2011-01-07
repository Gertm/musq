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
-compile(export_all).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

%% wrapper functions
-export([relay/2, change_area/2, set_name/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(WsPid) ->
	gen_server:start_link(?MODULE, [WsPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WsPid]) ->
	{ok, Tref} = timer:apply_interval(1000, 
									  gen_server, 
									  cast, 
									  [self(), {beat, self(), []}]),
	link(WsPid),
	process_flag(trap_exit, true),
	{ok, #plr{logged_in=false, wspid=WsPid, heartbeat=Tref}}.

%%--- HANDLE_CALL ---
handle_call({change_area, AreaAtom}, _From, State) ->
	{reply, ok, State#plr{area=AreaAtom}};
handle_call({set_name, PlayerName}, _From, State) ->
	%% ?show("Setting player name to: ~p~n",[PlayerName]),
	{reply, ok, State#plr{name=PlayerName}};
handle_call(get_visual_part, _From, State) ->
	%% ?show("to account: Player name = ~p~nWith state: ~p~n", [State#plr.name,State]),
	{reply, account:visual_part(State#plr.name), State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	?show("Got handle_call request I don't know: ~p~n",[_Request]),
	{reply, Reply, State}.

%%--- HANDLE_CAST ---
handle_cast({relay, Reply}, State) ->
	%% ?show("[player] Relaying ~p~n to ~p~n",[Reply, State#plr.wspid]),
	State#plr.wspid ! {reply, self(), Reply},	
	{noreply, State};
handle_cast({Fn, From, Params}, State) ->
	handle_request({Fn, From, Params}, State);
handle_cast({set_destination, {X, Y}}, State) ->
	{noreply, State#plr{destination={X, Y}}};
handle_cast(Any, State) ->
	?show("no idea what this is: ~p~n",[Any]),
	{noreply, State}.
 
%%--- HANDLE_INFO ---
handle_info({'EXIT', Pid, Reason}, State) ->
	if
		Pid == State#plr.wspid ->
			{stop, Reason, State};
		true ->
			{noreply, State}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%%--- TERMINATE & CODE_CHANGE ---
terminate(_Reason, State) ->
	timer:cancel(State#plr.heartbeat),
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
	%% ?show("~p Calling relay function ~p ~p~n",[self(), PlayerPid, Reply]),
	gen_server:cast(PlayerPid, {relay, Reply}).

change_area(PlayerPid, AreaAtom) ->
	gen_server:call(PlayerPid, {change_area, AreaAtom}).

set_name(PlayerPid, PlayerName) ->
	gen_server:call(PlayerPid, {set_name, PlayerName}).

get_visual_part(PlayerPid) ->
	gen_server:call(PlayerPid, get_visual_part).

set_destination(PlayerPid, {X, Y}) ->
	gen_server:cast(PlayerPid, {set_destination, {X, Y}}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

queue_request({Fn, From, Params}, #plr{rqueue=Q}=State) ->
	NewQ = queue:in({Fn, From, Params}, Q),
	State#plr{rqueue=NewQ}.

next_request(#plr{rqueue=Q}=State) ->
	{V, NewQ} = queue:out(Q),
	case V of
		empty ->
			{none, State#plr{rqueue=NewQ}};
		_ -> 
			{V, State#plr{rqueue=NewQ}}
	end.


handle_request({Fn, _From, Params}, #plr{}=State) ->
	%% all these functions need to return 
	%% gen_server:cast return values
	case Fn of
		logout -> handle_logout(Params, State);
		getFiles -> handle_getFiles(Params, State);
		createAccount -> handle_createAccount(Params, State);
		keepalive -> handle_keepalive(Params, State);
		talk -> handle_talk(Params, State);
		chatHistory -> handle_chatHistory(Params, State);
		beat -> handle_beat(Params, State);
		move -> handle_move(Params, State);
		_ -> 
			?show("Don't know this function: ~p(~p)~n", [Fn, Params]),
			{noreply, State}
	end;
handle_request(none, State) ->
	?show("handling none ~n",[]),
	{noreply, State}.


handle_logout(_Params, State) ->
	area:player_leave(State#plr.area, self(), State#plr.name),
	gen_server:call(world,{remove_player, State#plr.name}),
	%% gen_server still needs to shut down or it will linger (memleak)
	{stop, normal, State#plr{logged_in = false}}.

handle_createAccount(Params, State) ->
	%% when the player is logged in, this shouldn't work.
	?show("handling createaccount~n",[]),
	R = gen_server:call(world, {createAccount, Params}),
	State#plr.wspid ! {reply, self(), R},
	{noreply, State}.

handle_keepalive(_Params, State) ->
	State#plr.wspid ! {reply, self(), hlp:create_reply("keepalive",[])},
	{noreply, State}.

handle_talk(Params, State) ->
	%% ?show("Handling talk with params ~p~n",[Params]),
	Message = proplists:get_value("Message", Params),
	Name = State#plr.name,
	area:talk(State#plr.area, Name, Message),
	{noreply, State}.

handle_chatHistory(_Params, State) ->
	area:chat_history(State#plr.area, State#plr.wspid),
  	{noreply, State}.

handle_move(Params, State) ->
	{X, _} = string:to_integer(proplists:get_value("X", Params)),
	{Y, _} = string:to_integer(proplists:get_value("Y", Params)),
	NewState = State#plr{destination={X,Y}},
	{noreply, NewState}.

handle_getFiles(Params, State) ->
	case Params of
		[] -> ok;
		_ -> R = hlp:getFiles(Params),
			 State#plr.wspid ! {reply, self(), R}
	end,
	{noreply, State}.

handle_beat(_Params, #plr{destination=Dest, name=Name, area=Area}=State) ->
	%% for now, all this does is move the player.
	%% ?show("beat ",[]),
	case Dest of
		undefined ->
			{noreply, State};
		{Xdest, Ydest} ->
%%			?show("Going to ~p,~p~n",[Xdest, Ydest]),
			{X, Y} = area:player_pos(Area, Name),
%%			?show("Now at ~p,~p~n",[X,Y]),
			case hlp:is_same_pos({Xdest, Ydest}, {X, Y}) of
				true ->
					{noreply, State};
				false ->
					area:player_move(Area, self(), Name, Xdest, Ydest),	
					{noreply, State}
			end
	end.
