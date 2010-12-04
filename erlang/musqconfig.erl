-module('musqconfig').
-include("musq.hrl").
-compile(export_all).


out(A) ->
	error_logger:info_msg("musq-config being called"),
	Head = A#arg.headers,
	Host = Head#headers.host,
	io:format("musq-config ~s~n",[Host]),
	{html,"Bleh"}.
