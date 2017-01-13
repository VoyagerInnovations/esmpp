%% @private This contains the atom equivalents of all SMPP statuses
-module(esmpp_command_statuses).

-include("command_statuses.hrl").

-export([
  translate/1
]).

%% @private
translate(?ESME_ROK)              -> ok;
translate(?ESME_RINVMSGLEN)       -> invalid_message_length;
translate(?ESME_RINVCMDLEN)       -> invalid_command_length;
translate(?ESME_RINVCMDID)        -> invalid_command_id;
translate(?ESME_RINVBNDSTS)       -> invalid_bind_status;
translate(?ESME_RALYBND)          -> already_bound;
translate(?ESME_RINVPRTFLG)       -> invalid_priority_flag;
translate(?ESME_RINVREGDLVFLG)    -> invalid_reg_delivery_flag;
translate(?ESME_RSYSERR)          -> system_error;
translate(?ESME_RINVSRCADR)       -> invalid_src_addr;
translate(?ESME_RINVDSTADR)       -> invalid_dst_addr;
translate(?ESME_RINVMSGID)        -> invalid_message_id;
translate(?ESME_RBINDFAIL)        -> bind_failed;
translate(?ESME_RINVPASWD)        -> invalid_password;
translate(?ESME_RINVSYSID)        -> invalid_system_id;
translate(?ESME_RCANCELFAIL)      -> cancel_sm_failed;
translate(?ESME_RREPLACEFAIL)     -> replace_sm_failed;
translate(?ESME_RMSGQFUL)         -> message_queue_full;
translate(?ESME_RINVSERTYP)       -> invalid_service_type;
translate(?ESME_RINVNUMDESTS)     -> invalid_num_destinations;
translate(?ESME_RINVDLNAME)       -> invalid_dist_list_name;
translate(?ESME_RINVDESTFLAG)     -> invalid_dst_flag;
translate(?ESME_RINVSUBREP)       -> invalid_submit_replace;
translate(?ESME_RINVESMCLASS)     -> invalid_esm_class;
translate(?ESME_RCNTSUBDL)        -> unable_to_send_dist_list;
translate(?ESME_RSUBMITFAIL)      -> submit_sm_failed;
translate(?ESME_RINVSRCTON)       -> invalid_src_ton;
translate(?ESME_RINVSRCNPI)       -> invalid_src_npi;
translate(?ESME_RINVDSTTON)       -> invalid_dst_ton;
translate(?ESME_RINVDSTNPI)       -> invalid_dst_npi;
translate(?ESME_RINVSYSTYP)       -> invalid_system_type;
translate(?ESME_RINVREPFLAG)      -> invalid_replace_flag;
translate(?ESME_RINVNUMMSGS)      -> invalid_num_messages;
translate(?ESME_RTHROTTLED)       -> throttled;
translate(?ESME_RINVSCHED)        -> invalid_sched_delivery_time;
translate(?ESME_RINVEXPIRY)       -> invalid_validity;
translate(?ESME_RINVDFTMSGID)     -> predefined_message_not_found;
translate(?ESME_RX_T_APPN)        -> receiver_temporary_error;
translate(?ESME_RX_P_APPN)        -> receiver_permanent_error;
translate(?ESME_RX_R_APPN)        -> receiver_reject_message;
translate(?ESME_RQUERYFAIL)       -> query_sm_failed;
translate(?ESME_RINVOPTPARSTREAM) -> optional_param_error;
translate(?ESME_ROPTPARNOTALLWD)  -> optional_param_forbidden;
translate(?ESME_RINVPARLEN)       -> invalid_param_length; 
translate(?ESME_RMISSINGOPTPARAM) -> optional_param_missing;
translate(?ESME_RINVOPTPARAMVAL)  -> invalid_optional_param_val;
translate(?ESME_RDELIVERYFAILURE) -> delivery_failure;
translate(?ESME_RUNKNOWNERR)      -> unknown_error.
