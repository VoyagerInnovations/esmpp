%% @doc SMPP PDU Helper
%%
%% This module constructs and parses SMPP PDUs to and from octet formats
-module(esmpp_pdu).

%% Main exports
-export([
  bind/2,
  enquire_link/1,
  submit_sm/2
]).

%% Internally exposed
-export([
  pad2/1,
  pad4/1,
  iodata_to_cstring/2
]).

-include("constants.hrl").
-include("types.hrl").
-include("commands.hrl").

%% @doc Encode a bind PDU from a sequence number and a bind_pdu record
-spec bind(integer(), bind_pdu()) -> {pdu, binary()}.
bind(SeqNumInt, Bind) ->
  Status     = pad4(binary:encode_unsigned(?NULL)),
  SeqNum     = pad4(binary:encode_unsigned(SeqNumInt)),
  Binding    = pad4(binary:encode_unsigned(Bind#bind_pdu.binding)),
  Version    = binary:encode_unsigned(?SMPP_VERSION),
  AddrTon    = binary:encode_unsigned(Bind#bind_pdu.addr_ton),
  AddrNpi    = binary:encode_unsigned(Bind#bind_pdu.addr_npi),
  SystemId   = iodata_to_cstring(Bind#bind_pdu.system_id,   16),
  Password   = iodata_to_cstring(Bind#bind_pdu.password,    9),
  SystemType = iodata_to_cstring(Bind#bind_pdu.system_type, 13),
  AddrRange  = iodata_to_cstring(Bind#bind_pdu.addr_range,  41),
  TmpPDU     = <<Binding/binary, Status/binary, SeqNum/binary,
                 SystemId/binary, Password/binary, SystemType/binary,
                 Version/binary, AddrTon/binary, AddrNpi/binary,
                 AddrRange/binary>>,
  Length     = pad4(binary:encode_unsigned(size(TmpPDU) + 4)),
  PDU        = <<Length/binary, TmpPDU/binary>>,
  {pdu, PDU}.

%% @doc Encode an enquire_link PDU from a sequence number
-spec enquire_link(integer()) -> {pdu, binary()}.
enquire_link(SeqNumInt) ->
  Status     = pad4(binary:encode_unsigned(?NULL)),
  SeqNum     = pad4(binary:encode_unsigned(SeqNumInt)),
  Command    = pad4(binary:encode_unsigned(?ENQUIRE_LINK)),
  TmpPDU     = <<Command/binary, Status/binary, SeqNum/binary>>,
  Length     = pad4(binary:encode_unsigned(size(TmpPDU) + 4)),
  PDU        = <<Length/binary, TmpPDU/binary>>,
  {pdu, PDU}.

%% @doc Encode a submit_sm PDU from a sequence number and a submit_sm record
-spec submit_sm(integer(), submit_sm_pdu()) -> {pdu, binary()}.
submit_sm(SeqNumInt, SubmitSm) ->
  Status       = pad4(binary:encode_unsigned(?NULL)),
  SeqNum       = pad4(binary:encode_unsigned(SeqNumInt)),
  Command      = pad4(binary:encode_unsigned(?SUBMIT_SM)),
  SrcAddrTon   = binary:encode_unsigned(SubmitSm#submit_sm_pdu.src_addr_ton),
  SrcAddrNpi   = binary:encode_unsigned(SubmitSm#submit_sm_pdu.src_addr_npi),
  DstAddrTon   = binary:encode_unsigned(SubmitSm#submit_sm_pdu.dst_addr_ton),
  DstAddrNpi   = binary:encode_unsigned(SubmitSm#submit_sm_pdu.dst_addr_npi),
  EsmClass     = binary:encode_unsigned(SubmitSm#submit_sm_pdu.esm_class),
  ProtocolId   = binary:encode_unsigned(SubmitSm#submit_sm_pdu.protocol_id),
  PriorityFlag = binary:encode_unsigned(SubmitSm#submit_sm_pdu.priority_flag),
  RegDelivery  = binary:encode_unsigned(SubmitSm#submit_sm_pdu.reg_delivery),
  ReplaceFlag  = binary:encode_unsigned(SubmitSm#submit_sm_pdu.replace_flag),
  DataCoding   = binary:encode_unsigned(SubmitSm#submit_sm_pdu.data_coding),
  MsgId        = binary:encode_unsigned(SubmitSm#submit_sm_pdu.msg_id),
  SmLength     = binary:encode_unsigned(SubmitSm#submit_sm_pdu.sm_length),
  DeliveryTime = iodata_to_cstring(SubmitSm#submit_sm_pdu.delivery_time, 17),
  Validity     = iodata_to_cstring(SubmitSm#submit_sm_pdu.validity,      17),
  SrcAddr      = iodata_to_cstring(SubmitSm#submit_sm_pdu.src_addr,      21),
  DstAddr      = iodata_to_cstring(SubmitSm#submit_sm_pdu.dst_addr,      21),
  ServiceType  = iodata_to_cstring(SubmitSm#submit_sm_pdu.service_type,  6),
  ShortMessage = SubmitSm#submit_sm_pdu.short_message,
  OptParams    = SubmitSm#submit_sm_pdu.optional_params,
  Tlv          = pack_tlv(OptParams, ?SUBMIT_SM_OPT_PARAMS),
  TmpPDU       = <<Command/binary, Status/binary, SeqNum/binary,
                   ServiceType/binary, SrcAddrTon/binary, SrcAddrNpi/binary,
                   SrcAddr/binary, DstAddrTon/binary, DstAddrNpi/binary,
                   DstAddr/binary, EsmClass/binary, ProtocolId/binary,
                   PriorityFlag/binary, DeliveryTime/binary, Validity/binary,
                   RegDelivery/binary, ReplaceFlag/binary, DataCoding/binary,
                   MsgId/binary, SmLength/binary, ShortMessage/binary,
                   Tlv/binary>>,
  Length       = pad4(binary:encode_unsigned(size(TmpPDU) + 4)),
  PDU          = <<Length/binary, TmpPDU/binary>>,
  {pdu, PDU}.

%% ----------------------------------------------------------------------------
%% internal - but exposed nonetheless
%% ----------------------------------------------------------------------------

%% @private Used to convert erlang string to a C string
%% terminated by a NULL character
iodata_to_cstring(<<>>, _MaxLength)  -> <<0>>;
iodata_to_cstring(<<0>>, _MaxLength) -> <<0>>;
iodata_to_cstring(Data, MaxLength) when size(Data) >= MaxLength ->
  Limit = MaxLength - 1,
  DataBin = esmpp_format:ensure_binary(Data),
  <<ValidBin:Limit/binary, _Tail/binary>> = DataBin,
  <<ValidBin/binary, 0>>;
iodata_to_cstring(Data, _MaxLength) ->
  DataBin = esmpp_format:ensure_binary(Data),
  <<DataBin/binary, 0>>.

%% @private Pads erlang binary with max 2 octets
pad2(Bin)        -> pad2(Bin, big).
pad2(Bin, big)   -> <<0:((2 - (size(Bin) rem 4)) * 8), Bin/binary>>;
pad2(Bin, small) -> <<Bin/binary, 0:((2 - (size(Bin) rem 4)) * 8)>>.

%% @private Pads erlang binary with max 4 octets
pad4(Bin)        -> pad4(Bin, big).
pad4(Bin, big)   -> <<0:((4 - (size(Bin) rem 4)) * 8), Bin/binary>>;
pad4(Bin, small) -> <<Bin/binary, 0:((4 - (size(Bin) rem 4)) * 8)>>.

%% @private Packs optional params map to TLV binary. Will also strip unknown keys
pack_tlv(TmpParams, Keys) ->
  OptParams = maps:with(Keys, TmpParams),
  maps:fold(fun(Tag, Value, TmpTlv1) ->
    TmpTlv2 = esmpp_tags:pack(Tag, Value),
    <<TmpTlv1/binary, TmpTlv2/binary>> 
  end, <<>>, OptParams). 
