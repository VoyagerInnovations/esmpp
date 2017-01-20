-ifndef(constants).
-define(constants, true).

-define(SMPP_VERSION, 16#34).
-define(NULL,         16#00000000).

-define(SUBMIT_SM_OPT_PARAMS, [
  user_message_reference,
  source_port,
  source_addr_subunit,
  destination_port,
  dest_addr_subunit,
  sar_msg_ref_num,
  sar_total_segments,
  sar_segment_seqnum,
  more_messages_to_send,
  payload_type,
  message_payload,
  privacy_indicator,
  callback_num,
  callback_num_pres_ind,
  callback_num_atag,
  source_subaddress,
  dest_subaddress,
  user_response_code,
  display_time,
  sms_signal,
  ms_validity,
  ms_msg_wait_facilities,
  number_of_messages,
  alert_on_msg_delivery,
  language_indicator,
  its_reply_type,
  its_session_info,
  ussd_service_op
]).

-endif.
