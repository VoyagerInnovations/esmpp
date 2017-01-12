%% @private
-module(esmpp_tags).

-export([
  get_value/1
]).

%% @private
get_value(dest_addr_subunit)           -> 16#0005;
get_value(dest_network_type)           -> 16#0006;
get_value(dest_bearer_type)            -> 16#0007;
get_value(dest_telematics_id)          -> 16#0008;
get_value(source_addr_subunit)         -> 16#000D;
get_value(source_network_type)         -> 16#000E;
get_value(source_bearer_type)          -> 16#000F;
get_value(source_telematics_id)        -> 16#0010;
get_value(qos_time_to_live)            -> 16#0017;
get_value(payload_type)                -> 16#0019;
get_value(additional_status_info_text) -> 16#001D;
get_value(receipted_message_id)        -> 16#001E;
get_value(privacy_indicator)           -> 16#0201;
get_value(source_subaddress)           -> 16#0202;
get_value(dest_subaddress)             -> 16#0203;
get_value(user_message_reference)      -> 16#0204;
get_value(user_response_code)          -> 16#0205;
get_value(source_port)                 -> 16#020A;
get_value(destination_port)            -> 16#020B;
get_value(sar_msg_ref_num)             -> 16#020C;
get_value(language_indicator)          -> 16#020D;
get_value(sar_total_segments)          -> 16#020E;
get_value(sar_segment_seqnum)          -> 16#020F;
get_value(sc_interface_version)        -> 16#0210;
get_value(callback_num_pres_ind)       -> 16#0302;
get_value(callback_num_atag)           -> 16#0303;
get_value(number_of_messages)          -> 16#0304;
get_value(callback_num)                -> 16#0381;
get_value(dpf_result)                  -> 16#0420;
get_value(set_dpf)                     -> 16#0421;
get_value(ms_availability_status)      -> 16#0422;
get_value(network_error_code)          -> 16#0423;
get_value(message_payload)             -> 16#0424;
get_value(delivery_failure_reason)     -> 16#0425;
get_value(more_messages_to_send)       -> 16#0426;
get_value(message_state)               -> 16#0427;
get_value(ussd_service_op)             -> 16#0501;
get_value(display_time)                -> 16#1201;
get_value(sms_signal)                  -> 16#1203;
get_value(ms_validity)                 -> 16#1204;
get_value(alert_on_message_delivery)   -> 16#130C;
get_value(its_reply_type)              -> 16#1380;
get_value(its_session_info)            -> 16#1383.
