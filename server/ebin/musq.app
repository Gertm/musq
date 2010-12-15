{application, musq,
 [{description,"Multi User Space Quests"},
  {vsn,"0.1"},
  {modules, [musq,world,area,musqconfig,player,wshandle,musq_sup,hlp]},
  {registered, [world,musq_sup]},
  {applications, [kernel, stdlib]},
  {mod, {musq,[]}}]}.
