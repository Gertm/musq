{application, musq,
 [{description,"Multi User Space Quests"},
  {vsn,"0.1"},
  {modules, [musq,world,garea,musqconfig,player,wshandle,musq_sup]},
  {registered, [world,musq_sup]},
  {applications, [kernel, stdlib]},
  {mod, {musq,[]}}]}.
