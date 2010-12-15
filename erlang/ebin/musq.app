{application, musq,
 [{description,"Multi User Space Quests"},
  {vsn,"0.1"},
  {modules, [world,garea,musqconfig,player,wshandle]},
  {registered, [world]},
  {applications, [kernel, stdlib]},
  {mod, {musq_sup,[]}}]}.
