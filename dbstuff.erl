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
    mnesia:create_table(player,[{attributes, record_info(fields,player)},
				 {disc_copies,[node()]},
				{type, set}]),
    mnesia:create_table(room,[{attributes, record_info(fields,room)},
				 {disc_copies,[node()]},
				{type, set}]).

stop() ->
    application:stop(mnesia).


get_player(PlayerName) ->
    mnesia:transaction(fun() -> mnesia:read({player,PlayerName}) end).

save_player(_PlayerPid,Player) ->
   mnesia:transaction(fun() -> mnesia:write(Player) end).

get_room(RoomName) ->
    mnesia:transaction(fun() -> mnesia:read({room,RoomName}) end).

save_room(RoomState) ->
    mnesia:transaction(fun() -> mnesia:write(RoomState) end).
