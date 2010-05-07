-module(motd).
-export([motd/1]).
-include("telnetcolors.hrl").
motd(Socket) ->
    gen_tcp:send(Socket,"  ..      ..                    ..                         \r\n"),
    gen_tcp:send(Socket,"  ...    ...     ..   ..     ...          ........         \r\n"),
    gen_tcp:send(Socket,"  ...  .....     ..   ..    .             ..     ...       \r\n"),
    gen_tcp:send(Socket,"  ......  ..     ..   ..   ..            .         .       \r\n"),
    gen_tcp:send(Socket," ... ..   ..     ..   ..   ..           ..         ..      \r\n"),
    gen_tcp:send(Socket," ...      ...    ..   ..    ..          .           .      \r\n"),
    gen_tcp:send(Socket," ...      ...    ..   ..     .....     ..           .      \r\n"),
    gen_tcp:send(Socket," ...      ...    ..   ..         ..    .           ..      \r\n"),
    gen_tcp:send(Socket," ...      ...    ..   ..          .   ..      .    .       \r\n"),
    gen_tcp:send(Socket," ...      ...    ..   ..         ..   .       .......      \r\n"),
    gen_tcp:send(Socket," ...      ...    ..   ..       ...    .          .. ...    \r\n"),
    gen_tcp:send(Socket," ...      ...    ..  ...     ...      ....     ...    ..   \r\n"),
    gen_tcp:send(Socket,"  ..      ...     .....     ..           .......       .   \r\n"),
    gen_tcp:send(Socket,"  ..      ...               .                              \r\n"),
    gen_tcp:send(Socket,"\r\n").
