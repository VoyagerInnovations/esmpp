-ifndef(command_statuses).
-define(command_statuses, true).

-define(ESME_ROK,              16#00000000).
-define(ESME_RINVMSGLEN,       16#00000001).
-define(ESME_RINVCMDLEN,       16#00000002).
-define(ESME_RINVCMDID,        16#00000003).
-define(ESME_RINVBNDSTS,       16#00000004).
-define(ESME_RALYBND,          16#00000005).
-define(ESME_RINVPRTFLG,       16#00000006).
-define(ESME_RINVREGDLVFLG,    16#00000007).
-define(ESME_RSYSERR,          16#00000008).
-define(ESME_RINVSRCADR,       16#0000000A).
-define(ESME_RINVDSTADR,       16#0000000B).
-define(ESME_RINVMSGID,        16#0000000C).
-define(ESME_RBINDFAIL,        16#0000000D).
-define(ESME_RINVPASWD,        16#0000000E).
-define(ESME_RINVSYSID,        16#0000000F).
-define(ESME_RCANCELFAIL,      16#00000011).
-define(ESME_RREPLACEFAIL,     16#00000013).
-define(ESME_RMSGQFUL,         16#00000014).
-define(ESME_RINVSERTYP,       16#00000015).
-define(ESME_RINVNUMDESTS,     16#00000033).
-define(ESME_RINVDLNAME,       16#00000034).
-define(ESME_RINVDESTFLAG,     16#00000040).
-define(ESME_RINVSUBREP,       16#00000042).
-define(ESME_RINVESMCLASS,     16#00000043).
-define(ESME_RCNTSUBDL,        16#00000044).
-define(ESME_RSUBMITFAIL,      16#00000045).
-define(ESME_RINVSRCTON,       16#00000048).
-define(ESME_RINVSRCNPI,       16#00000049).
-define(ESME_RINVDSTTON,       16#00000050).
-define(ESME_RINVDSTNPI,       16#00000051).
-define(ESME_RINVSYSTYP,       16#00000053).
-define(ESME_RINVREPFLAG,      16#00000054).
-define(ESME_RINVNUMMSGS,      16#00000055).
-define(ESME_RTHROTTLED,       16#00000058).
-define(ESME_RINVSCHED,        16#00000061).
-define(ESME_RINVEXPIRY,       16#00000062).
-define(ESME_RINVDFTMSGID,     16#00000063).
-define(ESME_RX_T_APPN,        16#00000064).
-define(ESME_RX_P_APPN,        16#00000065).
-define(ESME_RX_R_APPN,        16#00000066).
-define(ESME_RQUERYFAIL,       16#00000067).
-define(ESME_RINVOPTPARSTREAM, 16#000000C0).
-define(ESME_ROPTPARNOTALLWD,  16#000000C1).
-define(ESME_RINVPARLEN,       16#000000C2).
-define(ESME_RMISSINGOPTPARAM, 16#000000C3).
-define(ESME_RINVOPTPARAMVAL,  16#000000C4).
-define(ESME_RDELIVERYFAILURE, 16#000000FE).
-define(ESME_RUNKNOWNERR,      16#000000FF).

-endif.