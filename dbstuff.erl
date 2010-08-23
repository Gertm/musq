-module(dbstuff).
-compile(export_all).

%% ---
%% little module to initialize db stuff
%% ---

-include("records.hrl").

start() ->
    mnesia:create_schema([node()]), %% we don't need this every time.
    application:start(mnesia).

init() ->
    mnesia:create_tables(player,[{attributes, record_info(fields,player)},
				 {disc_copies,[node()]},
				 {type, set}]).

stop() ->
    application:stop(mnesia).


get_player(PlayerPid) ->
    ok.

save_player(PlayerPid,Player) ->
   mnesia:transaction(fun() -> mnesia:write(Player) end).

