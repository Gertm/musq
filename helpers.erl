%%% @author Gert  Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert  Meulyzer
%%% @doc
%%% Helper functions module. For random functions not fitting anywhere else.
%%% @end
%%% Created : 11 Aug 2010 by Gert  Meulyzer <@G3rtm on Twitter>

-module(helpers).
%% API
-export([clean_name/1, room_process_name/1, wrap/2, recv_string/1,
	 room_name_from_filename/1, render_area_map/3]).
%% helpers
-export([add_word/3, add_words/3, replace_string/3, replace_list/3]).
-include("telnetcolors.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

clean_name(Name) ->
    re:replace(Name, " ", "_", [{return, list}]).

clean_name_test_() ->
    [?_assert(clean_name("test 1") =:= "test_1")].

room_process_name(Name) ->
    list_to_atom("room_"++clean_name(Name)).

wrap(String, Columns) ->
    %% string:tokens is built in, so it will be a lot faster.
    Words = string:tokens(String," "),
    add_words(Words, [""], Columns).

wrap_test_() ->
    [?_assert(wrap("", 10) =:= [""]),
     ?_assert(wrap("aa bb cc dd", 20) =:= ["aa bb cc dd"]),
     ?_assert(wrap("aa bb cc dd", 8) =:= ["aa bb cc", "dd"]),
     ?_assert(wrap("aa bb cc dd", 4) =:= ["aa", "bb", "cc", "dd"])].

recv_string(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    case binary_to_list(Data) of
		[X|_] when X > 127 -> recv_string(Socket);
		S ->
		    {R, _} = lists:split(length(S) - 2, S),
		    {tcp, Socket, R}
	    end;
	{tcp_closed, Socket} ->
	    io:format("~p Client Disconnected.~n", [erlang:localtime()]),
	    {tcp_closed, Socket};
	Other -> Other
    end.

room_name_from_filename(FileName) ->
    room_process_name(hd(string:tokens(filename:basename(FileName),"."))).

render_area_map(Data, Width, Height) ->
    EmptyLines = lists:duplicate(Height, string:copies(" ", Width)),
    XCenter = Width div 2 + 1,
    YCenter = Height div 2 + 1,
    lists:foldl(fun({X, Y, Z}, Acc) ->
			case Z =:= 0 of
			    true ->
				XReal = XCenter + X,
				YReal = YCenter - Y, %% swap Y axis
				OldLine = lists:nth(YReal, Acc),
				NewValue = case ((X =:= 0) and (Y =:= 0)) of
					       true -> ?yellow("*");
					       false -> "*"
					   end,
				NewLine = replace_string(OldLine, XReal, NewValue),
				replace_list(Acc, YReal, NewLine);
			    false -> Acc
			end
		end,
		EmptyLines,
		Data).

render_area_map_test_() ->
    [?_assert(render_area_map([], 3, 3) =:=
		  ["   ",
		   "   ",
		   "   "]),
     ?_assert(render_area_map([{0, 0, 0}, {-1, 0, 0}, {0, 1, 0}], 3, 3) =:=
		  [" * ",
		   "*"++?yellow("*")++" ",
		   "   "])].

%%%===================================================================
%%% helpers
%%%===================================================================

add_words([], Accumulator, _Columns) ->
    lists:reverse(Accumulator);
add_words([Word|Rest], Accumulator, Columns) ->
    NewAccumulator = add_word(Word, Accumulator, Columns),
    add_words(Rest, NewAccumulator, Columns).

add_word(Word, Accumulator, Columns) ->
    [CurrentLine|Rest] = Accumulator,
    case length(CurrentLine++" "++Word) > Columns of
	false ->
	    case length(CurrentLine) of
		0 -> [Word|Rest];
		_ -> [CurrentLine++" "++Word|Rest]
	    end;
	true -> [Word|[CurrentLine|Rest]]
    end.    

%% There's probably a better (standard?) way of doing this, but I didn't find anything.
replace_string(String, Index, NewValue) ->
    string:left(String, Index - 1)
	++ NewValue
	++ string:right(String, length(String) - Index).

replace_string_test_() ->
    [?_assert(replace_string("abcd", 3, "e") =:= "abed")].

%% There's probably a better (standard?) way of doing this, but I didn't find anything.
%% Also note that I couldn't reuse replace_string (or the other way around) as we want
%% to replace an element with a new element, whereas replace_string replaces a character
%% with a new substring.
replace_list(List, Index, NewValue) ->
    Indices = lists:seq(1, length(List)),
    ListWithIndices = lists:zip(List, Indices),
    [case I =:= Index of true -> NewValue; false -> E end || {E, I} <- ListWithIndices].

replace_list_test_() ->
    [?_assert(replace_list([1, 2, 3, 4], 3, 5) =:= [1, 2, 5, 4])].
