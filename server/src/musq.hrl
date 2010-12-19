%% every source file will probably need the eunit stuff.
%% most common things will go in here.
-include_lib("eunit/include/eunit.hrl").

-define(BASEPATH,"/home/gert/src/musq/").

-record(plr, {name ::string(),
			  position ::tuple(), %% {x,y}
			  area ::atom(),
			  pid ::pid(),
			  logged_in ::boolean()}).

-record(visualImage, {url   ::string(),
					  color ::string()}).

-record(visual, {username ::string(),
				 password ::string(),
				 email    ::string(),
				 images   ::[#visualImage{}]}).



-define(InfoMsg(Msg,Format), error_logger:info_msg(Msg,Format)).
