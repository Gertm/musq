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
		room, 
		equipment=[], 
		items=[], 
		experience}).

%% record for the rooms.
-record(room, {name, %% since they are all gen_servers, we can use list_to_atom to get the registered process name.
	       exits=[], %% probably in the form of: {"exitname",Pid} maybe not Pid, but some way of loading the file if needed.
	       desc=[],
	       npcs=[], %% {"Npc Name",Pid}
	       objects=[],
	       players=[], %% {"name",Pid}
	       messages=[]
	      }).

-record(area, {name,
	       rooms=[],
	       messages=[]}).
%% going to add more stuff to the area record once we start using them.
