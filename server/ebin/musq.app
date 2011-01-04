{application, musq,
 [{description,"Multi User Space Quests"},
  {vsn,"0.1"},
  {modules, [musq,world,area,musqconfig,player,wshandle,musq_sup,hlp,account,login,json,db,area_sup]},
  {registered, [world,musq_sup]},
  {applications, [kernel, stdlib, sasl]},
  {env, [{kernel, start_timer}]},
  {mod, {musq,[]}}]}.
