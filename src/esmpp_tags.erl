%% @private
-module(esmpp_tags).

-export([
  pack/2
]).

-include("constants.hrl").

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

%% @private
pack(dest_addr_subunit           = Tag, Value) -> pack_int1(Tag, Value);
pack(source_addr_subunit         = Tag, Value) -> pack_int1(Tag, Value);
pack(dest_network_type           = Tag, Value) -> pack_int1(Tag, Value);
pack(source_network_type         = Tag, Value) -> pack_int1(Tag, Value);
pack(dest_bearer_type            = Tag, Value) -> pack_int1(Tag, Value);
pack(source_bearer_type          = Tag, Value) -> pack_int1(Tag, Value);
pack(source_telematics_id        = Tag, Value) -> pack_int1(Tag, Value);
pack(payload_type                = Tag, Value) -> pack_int1(Tag, Value);
pack(ms_msg_wait_facilities      = Tag, Value) -> pack_int1(Tag, Value);
pack(privacy_indicator           = Tag, Value) -> pack_int1(Tag, Value);
pack(user_response_code          = Tag, Value) -> pack_int1(Tag, Value);
pack(language_indicator          = Tag, Value) -> pack_int1(Tag, Value);
pack(sar_total_segments          = Tag, Value) -> pack_int1(Tag, Value);
pack(sar_segment_seqnum          = Tag, Value) -> pack_int1(Tag, Value);
pack(sc_interface_version        = Tag, Value) -> pack_int1(Tag, Value);
pack(display_time                = Tag, Value) -> pack_int1(Tag, Value);
pack(ms_validity                 = Tag, Value) -> pack_int1(Tag, Value);
pack(dpf_result                  = Tag, Value) -> pack_int1(Tag, Value);
pack(set_dpf                     = Tag, Value) -> pack_int1(Tag, Value);
pack(ms_availability_status      = Tag, Value) -> pack_int1(Tag, Value);
pack(delivery_failure_reason     = Tag, Value) -> pack_int1(Tag, Value);
pack(more_messages_to_send       = Tag, Value) -> pack_int1(Tag, Value);
pack(message_state               = Tag, Value) -> pack_int1(Tag, Value);
pack(callback_num_pres_ind       = Tag, Value) -> pack_int1(Tag, Value);
pack(number_of_messages          = Tag, Value) -> pack_int1(Tag, Value);
pack(its_reply_type              = Tag, Value) -> pack_int1(Tag, Value);
pack(user_message_reference      = Tag, Value) -> pack_int2(Tag, Value);
pack(dest_telematics_id          = Tag, Value) -> pack_int2(Tag, Value);
pack(source_port                 = Tag, Value) -> pack_int2(Tag, Value);
pack(destination_port            = Tag, Value) -> pack_int2(Tag, Value);
pack(sar_msg_ref_num             = Tag, Value) -> pack_int2(Tag, Value);
pack(sms_signal                  = Tag, Value) -> pack_int2(Tag, Value);
pack(qos_time_to_live            = Tag, Value) -> pack_int4(Tag, Value);
pack(source_subaddress           = Tag, Value) -> pack_string(Tag, Value);
pack(dest_subaddress             = Tag, Value) -> pack_string(Tag, Value);
pack(network_error_code          = Tag, Value) -> pack_string(Tag, Value);
pack(message_payload             = Tag, Value) -> pack_string(Tag, Value);
pack(callback_num                = Tag, Value) -> pack_string(Tag, Value);
pack(callback_num_atag           = Tag, Value) -> pack_string(Tag, Value);
pack(its_session_info            = Tag, Value) -> pack_string(Tag, Value);
pack(ussd_service_op             = Tag, Value) -> pack_string(Tag, Value);
pack(receipted_message_id        = Tag, Value) -> pack_cstring(Tag, Value, 65);
pack(additional_status_info_text = Tag, Value) -> pack_cstring(Tag, Value, 256);
pack(alert_on_message_delivery   = Tag, _Value) -> pack_null(Tag).


%% @private
pack_null(Tag) ->
  Tag    = esmpp_pdu:pad2(get_value(Tag)),
  Length = esmpp_pdu:pad2(?NULL),
  <<Tag/binary, Length/binary>>.

%% @private
pack_int1(Tag, Value) ->
  Tag    = esmpp_pdu:pad2(get_value(Tag)),
  Length = esmpp_pdu:pad2(size(Value)),
  <<Tag/binary, Length/binary, Value/binary>>.

%% @private
pack_int2(Tag, TmpValue) ->
  Tag    = esmpp_pdu:pad2(get_value(Tag)),
  Value  = esmpp_pdu:pad2(TmpValue),
  Length = esmpp_pdu:pad2(size(Value)),
  <<Tag/binary, Length/binary, Value/binary>>.

%% @private
pack_int4(Tag, TmpValue) ->
  Tag    = esmpp_pdu:pad2(get_value(Tag)),
  Value  = esmpp_pdu:pad4(TmpValue),
  Length = esmpp_pdu:pad2(size(Value)),
  <<Tag/binary, Length/binary, Value/binary>>.

%% @private
pack_string(Tag, TmpValue) ->
  Tag    = esmpp_pdu:pad2(get_value(Tag)),
  Value  = esmpp_format:ensure_binary(TmpValue),
  Length = esmpp_pdu:pad2(size(Value)),
  <<Tag/binary, Length/binary, Value/binary>>.

%% @private
pack_cstring(Tag, TmpValue, MaxLength) ->
  Tag    = esmpp_pdu:pad2(get_value(Tag)),
  BinVal = esmpp_format:ensure_binary(TmpValue),
  Value  = esmpp_pdu:iodata_to_cstring(BinVal, MaxLength),
  Length = esmpp_pdu:pad2(size(Value)),
  <<Tag/binary, Length/binary, Value/binary>>.
