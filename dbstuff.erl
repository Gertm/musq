-module(dbstuff).
-compile(export_all).

%% ---
%% little module to initialize db stuff
%% ---

-include("records.hrl").

start() ->
    mnesia:create_schema([node()]), %% we probably don't need this every time.
    application:start(mnesia).

init() ->
    mnesia:create_tables(player,[{attributes, record_info(fields,player)},
				 {disc_copies,[node()]},
				 {type, set}]).

stop() ->
    application:stop(mnesia).


get_player(PlayerPid) ->
    mnesia

save_player(PlayerPid) ->
    ok.



