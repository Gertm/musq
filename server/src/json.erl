-module(json).
-author("vinnitu@gmail.com").
-vsn("1.0").
-export([decode/1, encode/1]).

decode(Q)->
	decode_wrap(mochijson2:decode(Q)).

decode_array([], A) ->
	A;
decode_array([H | T], A) ->
	decode_array(T, A ++ [decode_wrap(H)]).

decode_wrap(A) when is_list(A) ->
	{array, decode_array(A, [])};
decode_wrap({struct, Struct}) ->
	NewStruct =	lists:flatmap(fun({X, Y}) ->
		[{binary_to_list(X), decode_wrap(Y)}]
	end, Struct),
	{struct, NewStruct};
decode_wrap(B) when is_binary(B) ->
	binary_to_list(B);
decode_wrap(O) ->
	O.

encode(Q) ->
	mochijson2:encode(encode_wrap(Q)).

encode_array([], A) ->
	A;
encode_array([H | T], A) ->
	encode_array(T, A ++ [encode_wrap(H)]).

encode_wrap({array, A}) ->
	encode_array(A, []);
encode_wrap({struct, Struct}) ->
	NewStruct =	lists:flatmap(fun({X, Y}) ->
		[{X, encode_wrap(Y)}]
	end, Struct),
	{struct, NewStruct};
encode_wrap(S) when is_list(S) ->
	list_to_binary(S);
encode_wrap(O) ->
	O.

