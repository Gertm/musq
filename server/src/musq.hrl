%% every source file will probably need the eunit stuff.
%% most common things will go in here.
-include_lib("eunit/include/eunit.hrl").

-record(plr, {name ::string(),
			  position ::tuple(), %% {x,y}
			  area ::atom(),
			  pid ::pid(),
			  logged_in ::boolean()}).

-record(visual, {username ::string(),
				 password ::string(),
				 email    ::string(),
				 images   ::[term()]}).

-define(InfoMsg(Msg,Format), error_logger:info_msg(Msg,Format)).
