-module(dbstuff).
-compile(export_all).

%% ---
%% little module to initialize db stuff
%% ---

start() ->
    mnesia:create_schema([node()]),
    application:start(mnesia).

stop() ->
    application:stop(mnesia).



