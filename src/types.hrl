-ifndef(types).
-define(types, true).

-record(bind_pdu, {
  binding     = 0     :: integer(),
  system_id   = <<0>> :: iodata(),
  password    = <<0>> :: iodata(),
  system_type = <<0>> :: iodata(),
  addr_ton    = 0     :: integer(),
  addr_npi    = 0     :: integer(),
  addr_range  = <<0>> :: iodata()   
}).

-record(submit_sm_pdu, {
  service_type        = <<0>> :: iodata(),
  src_addr_ton        = 0     :: integer(),
  src_addr_npi        = 0     :: integer(),
  src_addr            = <<0>> :: iodata(),
  dst_addr_ton        = 0     :: integer(),
  dst_addr_npi        = 0     :: integer(),
  dst_addr            = <<0>> :: iodata(),
  esm_class           = 0     :: integer(),
  protocol_id         = 0     :: integer(),
  priority_flag       = 1     :: integer(),
  delivery_time       = <<0>> :: iodata(),
  validity            = <<0>> :: iodata(),
  reg_delivery        = 0     :: integer(),
  replace_flag        = 0     :: integer(),
  data_coding         = 0     :: integer(),
  msg_id              = 0     :: integer(),
  sm_length           = 0     :: integer(),
  short_message       = <<>>  :: iodata()
}).

-type(bind_pdu()      :: #bind_pdu{}).
-type(submit_sm_pdu() :: #submit_sm_pdu{}).

-endif.
