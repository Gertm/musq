%% Record definitions used in MUSQ.
%% also used to make the mnesia tables.

%% The State a player holds needs to contain these things:
%% player PID
%% player name
%% current room
%% equipment wearing
%% items carrying
%% experience
%% {}
-record(player,{pid,  %% <- this is nonsense when restarting the server. Need a better primary key
		name, 
		room, 
		equipment=[], 
		items=[], 
		experience}).

%% record for the rooms.
-record(room, {name,
	       exits=[],
	       desc=[],
	       npcs=[],
	       objects=[],
	       players=[],
	       messages=[]
	      }).
