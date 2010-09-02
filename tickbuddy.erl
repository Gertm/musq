%%% @author  <Randy Voet <crimson13>>
%%% @copyright (C) 2010, 
%%% @doc
%%%
%%% @end
%%% Created :  2 Sep 2010 by  <Randy Voet <crimson13>>

-module(tickbuddy).
-export([loop/2]).

loop(Pid, Timeout) ->
    receive
	{exit, Reason} ->
	    exit(Reason);
	{timeout, NewTimeout} ->
	    loop(Pid, NewTimeout)
    after Timeout ->
	    gen_server:cast(Pid, tick),
	    loop(Pid, Timeout)
    end.
