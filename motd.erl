-module(motd).
-export([motd/1]).
-include("telnetcolors.hrl").
motd(Socket) ->
    gen_tcp:send(Socket,"    .M       B        B     B         LN5.          rNS.    \r\n"),
    gen_tcp:send(Socket,"    .B7     JB       0B     B7      .BBBBBMi       MBBBB2   \r\n"),
    gen_tcp:send(Socket,"     BB     BB       BM     BB     iBBBMBBBB     :BBBBBBBB  \r\n"),
    gen_tcp:send(Socket,"     BBB   ZBB      jB      iB:    BBr   0B      BBBBBBBBBM \r\n"),
    gen_tcp:send(Socket,"    7BBBr  BBB:     BB       BM    BB     .     BBBBBBBBBBB \r\n"),
    gen_tcp:send(Socket,"    GBBBB 5BBBF     B2       BB    BB           BBBBBBBBBBB5\r\n"),
    gen_tcp:send(Socket,"    BBBBBBBBBBB    :Bi       0B    BBBi        YBBBBj.:GBBBM\r\n"),
    gen_tcp:send(Socket,"   .BBBBBBBBBBB    UBi       XB:    BBBBBG.    MBBB     JBBB\r\n"),
    gen_tcp:send(Socket,"   vBBBBBBBBBBBi   5Bv       GBr      vBBBBU   BBB:      BBB\r\n"),
    gen_tcp:send(Socket,"   vBBBBBBBBBBBi   5Bv       GBr      vBBBBU   BBB:      BBB\r\n"),
    gen_tcp:send(Socket,"   BBrBBBBBBr BZ   FBB       BBr  .X     rBB:  BBB       :BB\r\n"),
    gen_tcp:send(Socket,"   BB iBBBBB  BB   YBB:     iBB:  BBN.    2BB  BBE        BB\r\n"),
    gen_tcp:send(Socket,"  .BG  BBBBr  BB   iBBBi   iBBB.  BBBB7   rBB  GB0        BB\r\n"),
    gen_tcp:send(Socket,"  2B;  .BBB   YBr   BBBBBBBBBBB  iBBBB    BBB  7BB        BB\r\n"),
    gen_tcp:send(Socket,"  BB    GBr   .BZ   BBBBBBBBBBZ  iBBBBB1XBBBB   BB   7:  LBj\r\n"),
    gen_tcp:send(Socket,"  BB     BY    BB   vBBBBBBBBB.   BBBBBBBBBBF   BBq  YBBrBB \r\n"),
    gen_tcp:send(Socket," .BM     5L    BB    BBBBBBBBO    OBBBBBBBBB     BBU   BBBL \r\n"),
    gen_tcp:send(Socket," UBE           BBv    BBBBBBB      BBBBBBBB      .BBBMMBBB  \r\n"),
    gen_tcp:send(Socket," 7B:           kB:     5BBBY        iMBBBv         5BBBkBB; \r\n"),
    gen_tcp:send(Socket,"                                                        BBU \r\n"),
    gen_tcp:send(Socket,"                                                       :BBY \r\n"),
    gen_tcp:send(Socket,"\r\n").
