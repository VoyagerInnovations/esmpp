%% @copyright 2016 Voyager Innovations Inc.
%% @doc SMPP Client
%%
%% This module exposes a start_link/1 function that accepts bind parameters
%% and starts then registers an internal gen_server. A reference will then be
%% returned from the function.
%%
%% The start_link/1 completes the bind process and other exposed functions
%% implements capabilities of a SMPP client (version 3.4)
-module(esmpp).

-export([
  start_link/1,
  send_sms/4,
  send_sms/5
]).

-include("types.hrl").
-include("commands.hrl").

%% @doc Starts the connection to the SMPP server and does the binding process.
%%
%% `Options' is a map that should contain the following keys:
%%
%% <dt><b>`name'</b></dt>
%% <dd>If provided, the internal gen_server will be registered with this name.
%%     see gen_server:start_link/4</dd>
%% <dt><b>`mode'</b></dt>
%% <dd>Type of SMPP binding for the client</dd>
%% <dt><b>`host'</b></dt>
%% <dd>Hostname or IP address of the SMPP server</dd>
%% <dt><b>`port'</b></dt>
%% <dd>Port of the SMPP server</dd>
%% <dt><b>`system_id'</b></dt>
%% <dd>Identifier for the SMPP client</dd>
%% <dt><b>`password'</b></dt>
%% <dd>Password used to authenticate with the system_id</dd>
%% <dt><b>`system_type'</b></dt>
%% <dd>Identifier for the type of SMPP Client</dd>
%% <dt><b>`addr_ton'</b></dt>
%% <dd>Type of number for the SMPP server address.
%%     If unknown, can be unset. See <a href="#addr_ton">type of number values</a></dd>
%% <dt><b>`addr_npi'</b></dt>
%% <dd>Number planning indicator for the SMPP server address.
%%     If unknown, can be unset. See <a href="#addr_npi">number planning indicator values</a></dd>
%% <dt><b>`addr_range'</b></dt>
%% <dd>Used by tranceiver and receiver modes to specify which address range the
%%     SMPP client is ready to receive. If unknown, can be unset</dd>
%% <hr />
%% <div id="addr_ton">Type Of Number values:</div>
%% <dd> 
%%   <p>`0 - Unknown'</p>
%%   <p>`1 - International'</p>
%%   <p>`2 - National'</p>
%%   <p>`3 - Network Specific'</p>
%%   <p>`4 - Subscriber Number'</p>
%%   <p>`5 - Alphanumeric'</p>
%%   <p>`6 - Abbreviated'</p>
%% </dd>
%% <div id="addr_npi">Number Planning Indicator values:</div>
%% <dd>
%%   <p>`0  - Unknown'</p>
%%   <p>`1  - E163/E164'</p>
%%   <p>`3  - X.121'</p>
%%   <p>`4  - F.69'</p>
%%   <p>`6  - E.212'</p>
%%   <p>`8  - National Numbering Plan'</p>
%%   <p>`9  - Private Numbering Plan'</p>
%%   <p>`10 - ETSI DE/PS 3 01-3'</p>
%%   <p>`13 - IP'</p>
%%   <p>`18 - WAP Client ID'</p>
%% </dd>
-spec start_link(Options) -> {ok, pid()} | ignore | {error, term()} 
  when Options    :: #{ name          => ServerName ,
                        mode          => Mode       ,
                        host          => iodata()   ,
                        port          => integer()  ,
                        system_id     => iodata()   ,
                        password      => iodata()   ,
                        system_type   => iodata()   ,
                        addr_ton      => integer()  ,
                        addr_npi      => integer()  ,
                        addr_range    => iodata()   },
       Mode       :: transmitter | transceiver | receiver,
       ServerName :: {local,  Name       :: atom()} |
                     {global, GlobalName :: term()} |
                     {via,    Module     :: atom(), ViaName :: term()}.
start_link(Options) ->
  Name       = maps:get(name,        Options, '__undefined__'),
  Mode       = maps:get(mode,        Options, '__undefined__'), 
  SystemId   = maps:get(system_id,   Options, <<0>>),
  Password   = maps:get(password,    Options, <<0>>),
  AddrRange  = maps:get(addr_range,  Options, <<0>>),
  SystemType = maps:get(system_type, Options, <<0>>),
  AddrTon    = maps:get(addr_ton,    Options, 0),
  AddrNpi    = maps:get(addr_npi,    Options, 0),
  {binding, Binding} = get_binding(Mode),
  BindRecord    = #bind_pdu{
    binding     = Binding,
    system_id   = SystemId,
    password    = Password,
    system_type = SystemType,
    addr_ton    = AddrTon,
    addr_npi    = AddrNpi,
    addr_range  = AddrRange
  },
  start_link(Name, esmpp_worker, [Options, BindRecord]).

%% @private
start_link('__undefined__', Module, Args) ->
  gen_server:start_link(Module, Args, []);
start_link(Name, Module, Args) ->
  gen_server:start_link(Name, Module, Args, []).

%% @see send_sms/4
-spec send_sms(pid(), iodata(), iodata(), iodata()) -> {message_id, iodata()}.
send_sms(Conn, Sender, Destination, Message) ->
  send_sms(Conn, Sender, Destination, Message, []).

%% @doc Sends a short message to the specified MSISDN
%%
%% `Options' is an optional map that may contain the following keys:
%%
%% <dt><b>`service_type'</b></dt>
%% <dd>Indicates the SMS application service associated with the message</dd>
%% <dt><b>`protocol_id'</b></dt>
%% <dd>Network specific protocol identifier</dd>
%% <dt><b>`priority_flag'</b></dt>
%% <dd>Designates the priority level of the message.
%%     See <a href="priority_flag">priority flag values</a></dd>
%% <dt><b>`delivery_time'</b></dt>
%% <dd>Either the absolute date and time or relative time from the current 
%%     SMSC time which the delivery of the message is to be first attempted</dd>
%% <dt><b>`validity_period'</b></dt>
%% <dd>SMSC expiration time that indicates when the message should be discarded
%%     when the message is not delivered</dd>
%% <dt><b>`reg_delivery'</b></dt>
%% <dd>Indicates if a delivery receipt will be requested from the SMSC.
%%     See <a href="reg_delivery">delivery request values</a></dd>
%% <dt><b>`replace_flag'</b></dt>
%% <dd>Used to request the SMSC to replace a previously submitted message,
%%     that is pending delivery. The SMSC will replace the existing message
%%     if the source address, destination address, and service type match
%%     the same fields in the new message</dd>
%% <hr />
%% <div id="priority_flag">Priority Flag values:</div>
%% <dd> 
%%   <p>`0 - Level 0 (lowest) priority'</p>
%%   <p>`1 - Level 1 priority'</p>
%%   <p>`2 - Level 2 priority'</p>
%%   <p>`3 - Level 3 (highest) priority'</p>
%% </dd>
%% <div id="reg_delivery">Delivery Request values:</div>
%% <dd> 
%%   <p>`0 - No delivery receipt requested'</p>
%%   <p>`1 - Delivery receipt requested (success or failure)'</p>
%%   <p>`2 - Delivery receipt requested (failure only)'</p>
%% </dd>
-spec send_sms(pid(), iodata(), iodata(), iodata(), Options) -> {message_id, iodata()}
  when Options :: #{ service_type  => iodata()  ,
                     protocol_id   => integer() ,
                     priority_flag => integer() ,
                     delivery_time => iodata()  ,
                     validity      => iodata()  ,
                     reg_delivery  => integer() ,
                     replace_flag  => integer() }.
send_sms(Conn, Sender, Destination, TmpMessage, Options) ->
  ServiceType  = maps:get(service_type,  Options, <<0>>),
  ProtocolId   = maps:get(protocol_id,   Options, 0),
  PriorityFlag = maps:get(priority_flag, Options, 0),
  DeliveryTime = maps:get(delivery_time, Options, <<0>>),
  Validity     = maps:get(validity,      Options, <<0>>),
  RegDelivery  = maps:get(reg_delivery,  Options, 0),
  ReplaceFlag  = maps:get(replace_flag,  Options, 0),
  DataCoding   = get_data_coding(TmpMessage),
  SrcAddrTon   = get_ton(esmpp_format:ensure_binary(Sender)),
  SrcAddrNpi   = get_npi(esmpp_format:ensure_binary(Sender)),
  DstAddrTon   = get_ton(esmpp_format:ensure_binary(Destination)),
  DstAddrNpi   = get_npi(esmpp_format:ensure_binary(Destination)),
  Message      = encode(esmpp_format:ensure_binary(TmpMessage), DataCoding),
  SubmitSm = #submit_sm_pdu{
    service_type  = ServiceType,
    src_addr_ton  = SrcAddrTon,
    src_addr_npi  = SrcAddrNpi,
    src_addr      = Sender,
    dst_addr_ton  = DstAddrTon,
    dst_addr_npi  = DstAddrNpi,
    dst_addr      = Destination,
    data_coding   = DataCoding,
    protocol_id   = ProtocolId,
    priority_flag = PriorityFlag,
    delivery_time = DeliveryTime,
    validity      = Validity,
    reg_delivery  = RegDelivery,
    replace_flag  = ReplaceFlag,
    sm_length     = size(Message),
    short_message = Message
  },
  gen_server:call(Conn, {submit_sm, SubmitSm, Options}).

%% ----------------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------------

%% @private
get_binding(transmitter) -> {binding, ?BIND_TRANSMITTER};
get_binding(transceiver) -> {binding, ?BIND_TRANSCEIVER};
get_binding(receiver)    -> {binding, ?BIND_RECEIVER}.

%% @private
get_ton(Number) ->
  get_ton(Number, get_address_type(Number)).
get_ton(Number,  digit) when size(Number) < 3 orelse  size(Number) > 15 -> 0;
get_ton(Number,  digit) when size(Number) > 2 andalso size(Number) < 9  -> 3;
get_ton(Number,  digit) when size(Number) > 8 andalso size(Number) < 16 -> 1;
get_ton(_Number, alpha) -> 5.

%% @private
get_npi(Number) ->
  get_npi(Number, get_address_type(Number)).
get_npi(Number,  digit) when size(Number) < 3 orelse  size(Number) > 15 -> 0;
get_npi(Number,  digit) when size(Number) > 2 andalso size(Number) < 9  -> 0;
get_npi(Number,  digit) when size(Number) > 8 andalso size(Number) < 16 -> 1;
get_npi(_Number, alpha) -> 0.

%% @private
get_address_type(Number) ->
  get_address_type([N || <<N:1/binary>> <= Number], digit).
get_address_type([], digit) -> digit;
get_address_type([Char | Rest], _Any) -> 
  case binary:decode_unsigned(Char) of
    Val when Val =:= 43 orelse Val > 47 andalso Val < 58 ->
      get_address_type(Rest, digit);
    _Val ->
      alpha
  end.

%% @private
get_data_coding(Message) ->
  BinMessage = esmpp_format:ensure_binary(Message), 
  is_basic_latin([N || <<N:1/binary>> <= BinMessage]).
  
%% @private
is_basic_latin([]) -> 0;
is_basic_latin([Char | Rest]) ->
  case binary:decode_unsigned(Char) of
    Val when Val < 128 ->
      is_basic_latin(Rest);
    _Val ->
      8
  end.

%% @private
encode(Message, 0) ->
  gsm0338:from_utf8(Message);
encode(Message, 8) ->
  unicode:characters_to_binary(Message, utf8, utf16). 
