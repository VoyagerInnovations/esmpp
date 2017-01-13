%% @private This contains the atom equivalents of all SMPP statuses
-module(esmpp_status).

-include("command_statuses.hrl").

-export([
  to_atom/1
]).

%% @private
to_atom(?ESME_ROK)              -> ok;
to_atom(?ESME_RINVMSGLEN)       -> invalid_message_length;
to_atom(?ESME_RINVCMDLEN)       -> invalid_command_length;
to_atom(?ESME_RINVCMDID)        -> invalid_command_id;
to_atom(?ESME_RINVBNDSTS)       -> invalid_bind_status;
to_atom(?ESME_RALYBND)          -> already_bound;
to_atom(?ESME_RINVPRTFLG)       -> invalid_priority_flag;
to_atom(?ESME_RINVREGDLVFLG)    -> invalid_reg_delivery_flag;
to_atom(?ESME_RSYSERR)          -> system_error;
to_atom(?ESME_RINVSRCADR)       -> invalid_src_addr;
to_atom(?ESME_RINVDSTADR)       -> invalid_dst_addr;
to_atom(?ESME_RINVMSGID)        -> invalid_message_id;
to_atom(?ESME_RBINDFAIL)        -> bind_failed;
to_atom(?ESME_RINVPASWD)        -> invalid_password;
to_atom(?ESME_RINVSYSID)        -> invalid_system_id;
to_atom(?ESME_RCANCELFAIL)      -> cancel_sm_failed;
to_atom(?ESME_RREPLACEFAIL)     -> replace_sm_failed;
to_atom(?ESME_RMSGQFUL)         -> message_queue_full;
to_atom(?ESME_RINVSERTYP)       -> invalid_service_type;
to_atom(?ESME_RINVNUMDESTS)     -> invalid_num_destinations;
to_atom(?ESME_RINVDLNAME)       -> invalid_dist_list_name;
to_atom(?ESME_RINVDESTFLAG)     -> invalid_dst_flag;
to_atom(?ESME_RINVSUBREP)       -> invalid_submit_replace;
to_atom(?ESME_RINVESMCLASS)     -> invalid_esm_class;
to_atom(?ESME_RCNTSUBDL)        -> unable_to_send_dist_list;
to_atom(?ESME_RSUBMITFAIL)      -> submit_sm_failed;
to_atom(?ESME_RINVSRCTON)       -> invalid_src_ton;
to_atom(?ESME_RINVSRCNPI)       -> invalid_src_npi;
to_atom(?ESME_RINVDSTTON)       -> invalid_dst_ton;
to_atom(?ESME_RINVDSTNPI)       -> invalid_dst_npi;
to_atom(?ESME_RINVSYSTYP)       -> invalid_system_type;
to_atom(?ESME_RINVREPFLAG)      -> invalid_replace_flag;
to_atom(?ESME_RINVNUMMSGS)      -> invalid_num_messages;
to_atom(?ESME_RTHROTTLED)       -> throttled;
to_atom(?ESME_RINVSCHED)        -> invalid_sched_delivery_time;
to_atom(?ESME_RINVEXPIRY)       -> invalid_validity;
to_atom(?ESME_RINVDFTMSGID)     -> predefined_message_not_found;
to_atom(?ESME_RX_T_APPN)        -> receiver_temporary_error;
to_atom(?ESME_RX_P_APPN)        -> receiver_permanent_error;
to_atom(?ESME_RX_R_APPN)        -> receiver_reject_message;
to_atom(?ESME_RQUERYFAIL)       -> query_sm_failed;
to_atom(?ESME_RINVOPTPARSTREAM) -> optional_param_error;
to_atom(?ESME_ROPTPARNOTALLWD)  -> optional_param_forbidden;
to_atom(?ESME_RINVPARLEN)       -> invalid_param_length; 
to_atom(?ESME_RMISSINGOPTPARAM) -> optional_param_missing;
to_atom(?ESME_RINVOPTPARAMVAL)  -> invalid_optional_param_val;
to_atom(?ESME_RDELIVERYFAILURE) -> delivery_failure;
to_atom(?ESME_RUNKNOWNERR)      -> unknown_error.
