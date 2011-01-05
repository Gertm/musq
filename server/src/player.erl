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
	{ok, #plr{logged_in=false, wspid=WsPid, rqueue=queue:new()}}.

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
handle_cast({logout, _WsPid, _Params}, State) ->
	area:player_leave(State#plr.area, self(), State#plr.name),
	gen_server:call(world,{remove_player, State#plr.name}),
	%% gen_server still needs to shut down or it will linger (memleak)
	{stop, normal, State#plr{logged_in = false}};
handle_cast({getFiles, From, Params},State) ->
	case Params of
		[] -> ok;
		_ -> R = hlp:getFiles(Params),
			 From ! {reply, self(), R}
	end,
	{noreply, State};
handle_cast({createAccount, From, Params}, State) ->
	%% when the player is logged in, this shouldn't work.
	?show("handling createaccount~n",[]),
	R = gen_server:call(world, {createAccount, Params}),
	From ! {reply, self(), R},
	{noreply, State};
handle_cast({keepalive, From, []},State) ->
	From ! {reply, self(), hlp:create_reply("keepalive",[])},
	{noreply, State};
handle_cast({relay, Reply}, State) ->
	%% ?show("[player] Relaying ~p~n to ~p~n",[Reply, State#plr.wspid]),
	State#plr.wspid ! {reply, self(), Reply},	
	{noreply, State};
handle_cast({talk, _From, Params}, State) ->
	%% ?show("Handling talk with params ~p~n",[Params]),
	Message = proplists:get_value("Message", Params),
	Name = State#plr.name,
	area:talk(State#plr.area, Name, Message),
	{noreply, State};
handle_cast({chatHistory, _From, _}, State) ->
	area:chat_history(State#plr.area, State#plr.wspid),
  	{noreply, State};
handle_cast({move, _From, Params}, State) ->
	{X, _} = string:to_integer(proplists:get_value("X", Params)),
	{Y, _} = string:to_integer(proplists:get_value("Y", Params)),
	area:player_move(State#plr.area, self(), State#plr.name, X, Y),
	{noreply, State};
handle_cast({queue, Req}, #plr{rqueue=Queue}=State) ->
	?show("Queuing: ~p~n",[Req]),
	{noreply, State#plr{rqueue=[Req | Queue]}};
handle_cast(Any, State) ->
	?show("no idea what this is: ~p~n",[Any]),
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

change_area(PlayerPid, AreaAtom) ->
	gen_server:call(PlayerPid, {change_area, AreaAtom}).

set_name(PlayerPid, PlayerName) ->
	gen_server:call(PlayerPid, {set_name, PlayerName}).

get_visual_part(PlayerPid) ->
	gen_server:call(PlayerPid, get_visual_part).

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


