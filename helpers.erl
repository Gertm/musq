%%% @author Gert  Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2010, Gert  Meulyzer
%%% @doc
%%% Helper functions module. For random functions not fitting anywhere else.
%%% @end
%%% Created : 11 Aug 2010 by Gert  Meulyzer <@G3rtm on Twitter>

-module(helpers).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

clean_name(Name) ->
    re:replace(Name, " ", "_", [{return, list}]).

clean_name_test_() ->
    [?_assert(clean_name("test 1") =:= "test_1")].

wrap(String, Columns) ->
    Words = split_words(String, $ ),
    add_words(Words, [""], Columns).

wrap_test_() ->
    [?_assert(wrap("", 10) =:= [""]),
     ?_assert(wrap("aa bb cc dd", 20) =:= ["aa bb cc dd"]),
     ?_assert(wrap("aa bb cc dd", 8) =:= ["aa bb cc", "dd"]),
     ?_assert(wrap("aa bb cc dd", 4) =:= ["aa", "bb", "cc", "dd"])].

split_words(String, Separator) ->
    split_words(String, Separator, "", []).

split_words(String, Separator, CurrentWord, Words) ->
    case String of
	"" -> lists:reverse([CurrentWord|Words]);
	[Separator|Rest] -> split_words(Rest, Separator, "", [CurrentWord|Words]);
	[Char|Rest] -> split_words(Rest, Separator, CurrentWord++[Char], Words)
    end.

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

%% TODO: Find a better name.
foreachex(Fun, [First|Rest]) ->
    Fun(First, true),
    lists:foreach(fun(Elem) ->
			  Fun(Elem, false)
		  end,
		  Rest);
foreachex(Fun, []) ->
    lists:foreach(Fun, []).
