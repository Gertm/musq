%% every source file will probably need the eunit stuff.
%% most common things will go in here.
-include_lib("eunit/include/eunit.hrl").

-define(BASEPATH,"/home/gert/src/musq/").
-define(AREAPATH, ?BASEPATH++"server/areas/").

-record(plr, {name ::string(),
			  position ::tuple(), %% {x,y}
			  area ::atom(),
			  pid ::pid(),
			  wspid ::pid(),
			  rqueue=queue:new() ::queue(),
			  destination ::tuple(),
			  heartbeat ::term(),
			  logged_in ::boolean()}).

%% this one is for mnesia
-record(player, {name ::string(),
				 position ::tuple(),
				 area ::atom(),
				 pid ::pid(),
				 logged_in ::boolean()}).


-record(visualImage, {url   ::string(),
					  color ::string()}).

-record(visual, {username ::string(),
				 password ::string(),
				 email    ::string(),
				 images   ::[#visualImage{}]}).

-define(InfoMsg(Msg,Format), io:format(Msg,Format)).

-define(show(Msg, Format), io:format(Msg, Format)).

-record(account, {username ::string(),
				  password ::string(),
				  email    ::string(),
				  images   ::[#visualImage{}]}).
