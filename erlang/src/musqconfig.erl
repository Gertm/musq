-module('musqconfig').
-include("musq.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-compile(export_all).


out(A) ->
	Head = A#arg.headers,
	Host = Head#headers.host,
	{html,"var musq_websocket_url = \""++Host++"\";"}.
