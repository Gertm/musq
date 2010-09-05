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
    mnesia:create_table(player, [{attributes, record_info(fields, player)}, 
				 {disc_copies, [node()]}, 
				{type, set}]), 
    mnesia:create_table(room, [{attributes, record_info(fields, room)}, 
				 {disc_copies, [node()]}, 
			      {type, set}]), 
    mnesia:create_table(namepid, [{attributes, record_info(fields, namepid)}]).
%% potential problem: a player naming himself the same way a room is called.
%% need to make sure this can't happen?

stop() ->
    application:stop(mnesia).


get_player(PlayerName) ->
    mnesia:transaction(fun() -> mnesia:read({player, PlayerName}) end).

save_player(_PlayerPid, Player) ->
   mnesia:transaction(fun() -> mnesia:write(Player) end).

get_room(RoomName) ->
    mnesia:transaction(fun() -> mnesia:read({room, RoomName}) end).

save_room(RoomState) ->
    mnesia:transaction(fun() -> mnesia:write(RoomState) end).

%% not sure if we need these next 3 functions, because since a room and a player
%% are both going to be gen_servers, they will be registered in the global process
%% database. Also, mnesia is not really meant for simple key-value stuff.

get_pid(Name) ->
    {atomic, Result} = mnesia:transaction(fun() -> mnesia:read({namepid, Name}) end), 
    case Result of
	[] -> not_found;
	[NamePid] -> NamePid#namepid.pid
    end.

save_namepidrec(NamePidRecord) ->
    mnesia:transaction(fun() -> mnesia:write(NamePidRecord) end).

save_namepid(Name, Pid) ->
    save_namepidrec(#namepid{name=Name, pid=Pid}).
