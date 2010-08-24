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
-record(player,{name,
		pid,
		room, 
		equipment=[], 
		items=[], 
		experience}).

%% record for the rooms.
-record(room, {name, %% since they are all gen_servers, we can use list_to_atom to get the registered process name.
	       exits=[],
	       desc=[],
	       npcs=[],
	       objects=[],
	       players=[],
	       messages=[]
	      }).

-record(area, {name,
	       rooms=[],
	       messages=[]}).
%% going to add more stuff to the area record once we start using them.
