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
		experience,
		controllerPid}).

%% record for the rooms.
-record(room, {name, %% maybe we should extend later to make this store the PID too.
	       exits=[], %% a list of atoms, corresponding directly to the registered processes with the same name.
	       desc=[], %% a list of paragraphs describing the room
	       npcs=[], %% {"Npc Name",Pid}
	       objects=[],
	       players=[], %% {"name",Pid}
	       messages=[]
	      }).

-record(area, {name,
	       rooms=[],
	       messages=[]}).

-record(namepid, {name,pid}).

%% going to add more stuff to the area record once we start using them.
