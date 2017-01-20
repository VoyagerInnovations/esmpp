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
  send_sms/5,
  send_sms/6,
  callback/3
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

%% @see send_sms/5
-spec send_sms(pid(), iodata(), iodata(), iodata()) -> [{message_id, iodata()}] | {error, atom()}.
send_sms(C, Sender, Destination, Message) ->
  send_sms(C, Sender, Destination, Message, #{}).

%% @see send_sms/6
-spec send_sms(pid(), iodata(), iodata(), iodata(), map()) -> [{message_id, iodata()}] | {error, atom()}.
send_sms(C, Sender, Destination, Message, Options) ->
  send_sms(C, Sender, Destination, Message, Options, #{}).

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
%%     See <a href="#priority_flag">priority flag values</a></dd>
%% <dt><b>`delivery_time'</b></dt>
%% <dd>Either the absolute date and time or relative time from the current 
%%     SMSC time which the delivery of the message is to be first attempted</dd>
%% <dt><b>`validity_period'</b></dt>
%% <dd>SMSC expiration time that indicates when the message should be discarded
%%     when the message is not delivered</dd>
%% <dt><b>`reg_delivery'</b></dt>
%% <dd>Indicates if a delivery receipt will be requested from the SMSC.
%%     See <a href="#reg_delivery">delivery request values</a></dd>
%% <dt><b>`replace_flag'</b></dt>
%% <dd>Used to request the SMSC to replace a previously submitted message,
%%     that is pending delivery. The SMSC will replace the existing message
%%     if the source address, destination address, and service type match
%%     the same fields in the new message</dd>
%%
%% `OptionalParameters' is another optional map that goes to the <em>optional
%% parameters</em> part of the PDU. For this particular operation, the following
%% keys can be used:
%%
%% <dt><b>`user_message_reference'</b></dt>
%% <dd>Reference number assigned by the SMPP clientto the message</dd>
%% <dt><b>`source_port'</b></dt>
%% <dd> Indicates the application port number associated with the source 
%%      address of the message. Should be present for WAP applications</dd>
%% <dt><b>`source_addr_subunit'</b></dt>
%% <dd>The subcomponent in the destination device which created the user data.
%%     See <a href="#subunit">subunit values</a></dd>
%% <dt><b>`destination_port'</b></dt>
%% <dd> Indicates the application port number associated with the destination 
%%      address of the message. Should be present for WAP applications</dd>
%% <dt><b>`dest_addr_subunit'</b></dt>
%% <dd>The subcomponent in the destination device for which the user data
%%     is intended. See <a href="#subunit">subunit values</a></dd>
%% <dt><b>`sar_msg_ref_num'</b></dt>
%% <dd>Reference number of a particular concatenated message</dd>
%% <dt><b>`sar_total_segments'</b></dt>
%% <dd>Total number of messages within the concatenated message</dd>
%% <dt><b>`sar_segment_seqnum'</b></dt>
%% <dd>Sequence number of a particular message fragment within the concatenated
%%     message</dd>
%% <dt><b>`more_messages_to_send'</b></dt>
%% <dd>Indicates that there are more messages to follow</dd>
%% <dt><b>`payload_type'</b></dt>
%% <dd>Defines the type of payload. See <a href="#payload_type">payload
%%     types</a></dd>
%% <dt><b>`message_payload'</b></dt>
%% <dd>Extended short message user data</dd>
%% <dt><b>`privacy_indicator'</b></dt>
%% <dd>Indicates the level of privacy associated with the message.
%%     See <a href="#privacy_indicator">privacy indicator values</a></dd>
%% <dt><b>`callback_num'</b></dt>
%% <dd>Callback number associated with the message</dd>
%% <dt><b>`callback_num_pres_ind'</b></dt>
%% <dd>Defines the callback number presentation and screening</dd>
%% <dt><b>`callback_num_atag'</b></dt>
%% <dd>Associates a displayable alphanumeric tag with the callback number</dd>
%% <dt><b>`source_subaddress'</b></dt>
%% <dd>Subaddress of the message originator</dd>
%% <dt><b>`dest_subaddress'</b></dt>
%% <dd>Subaddress of the message destination</dd>
%% <dt><b>`user_response_code'</b></dt>
%% <dd>A user response code. <em>Implementation specific</em></dd>
%% <dt><b>`display_time'</b></dt>
%% <dd> Provides the receiving MS with a display time associated with the
%%      message</dd>
%% <dt><b>`sms_signal'</b></dt>
%% <dd>Indicates the alerting mechanism when the message is received by an MS</dd>
%% <dt><b>`ms_validity'</b></dt>
%% <dd>Indicates the validity information for the message to the receiving MS.
%%     See <a href="#ms_validity">MS validity values</a></dd>
%% <dt><b>`ms_msg_wait_facilities'</b></dt>
%% <dd>Controls the indication and specifies the message type at the MS</dd>
%% <dt><b>`number_of_messages'</b></dt>
%% <dd>Number of messages stored in a mail box</dd>
%% <dt><b>`alert_on_msg_delivery'</b></dt>
%% <dd>Requests an MS alert signal be invoked on message delivery</dd>
%% <dt><b>`language_indicator'</b></dt>
%% <dd>Indicates the language of an alphanumeric text. See 
%%     <a href="#language_indicator">language indicator values</a></dd>
%% <dt><b>`its_reply_type'</b></dt>
%% <dd>The MS user's reply method to a SMS delivery message received from
%%     the network. See <a href="#its_reply_type">reply type values</a></dd>
%% <dt><b>`its_session_info'</b></dt>
%% <dd>Session control information for Interactive Teleservice</dd>
%% <dt><b>`ussd_service_op'</b></dt>
%% <dd>Identifies the required USSD Service type when interfacing to
%%     a USSD system. See <a href="#ussd_service_op">USSD service operation
%%     values</a></dd>
%% 
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
%% <div id="subunit">Subunit values:</div>
%% <dd> 
%%   <p>`0 - Unknown (Default)'</p>
%%   <p>`1 - MS Display'</p>
%%   <p>`2 - Mobile Equipment'</p>
%%   <p>`3 - Smart Card 1'</p>
%%   <p>`4 - External Unit 1'</p>
%% </dd>
%% <div id="payload_type">Payload Type values:</div>
%% <dd> 
%%   <p>`0 - Default'</p>
%%   <p>`1 - WCMP formatted'</p>
%% </dd>
%% <div id="privacy_indicator">Privacy Indicator values:</div>
%% <dd> 
%%   <p>`0 - Privacy level 0 (Not Restricted - Default)'</p>
%%   <p>`1 - Privacy level 1 (Restricted)'</p>
%%   <p>`2 - Privacy level 2 (Confidential)'</p>
%%   <p>`3 - Privacy level 3 (Secret)'</p>
%% </dd>
%% <div id="ms_validity">MS Validity values:</div>
%% <dd> 
%%   <p>`0 - Store Indefinitely (Default)'</p>
%%   <p>`1 - Power Down'</p>
%%   <p>`2 - SID based registration area'</p>
%%   <p>`3 - Display Only'</p>
%% </dd>
%% <div id="language_indicator">Language Indicator values:</div>
%% <dd> 
%%   <p>`0 - Unspecified (Default)'</p>
%%   <p>`1 - English'</p>
%%   <p>`2 - French'</p>
%%   <p>`3 - Spanish'</p>
%%   <p>`4 - German'</p>
%%   <p>`5 - Portuguese'</p>
%% </dd>
%% <div id="its_reply_type">Reply Type values:</div>
%% <dd> 
%%   <p>`0 - Digit'</p>
%%   <p>`1 - Number'</p>
%%   <p>`2 - Telephone No.'</p>
%%   <p>`3 - Password'</p>
%%   <p>`4 - Character Line'</p>
%%   <p>`5 - Menu'</p>
%%   <p>`6 - Date'</p>
%%   <p>`7 - Time'</p>
%%   <p>`8 - Continue'</p>
%% </dd>
%% <div id="ussd_service_op">USSD Service Operation values:</div>
%% <dd> 
%%   <p>`0  - PSSD indication'</p>
%%   <p>`1  - PSSR indication'</p>
%%   <p>`2  - USSR request'</p>
%%   <p>`3  - USSN request'</p>
%%   <p>`16 - PSSD response'</p>
%%   <p>`17 - PSSR response'</p>
%%   <p>`18 - USSR confirm'</p>
%%   <p>`19 - USSN confirm'</p>
%% </dd>
-spec send_sms(pid(), iodata(), iodata(), iodata(), Options, OptionalParams) -> [{message_id, iodata()}] | {error, atom()}
  when Options        :: #{ service_type           => iodata()  ,
                            protocol_id            => integer() ,
                            priority_flag          => integer() ,
                            delivery_time          => iodata()  ,
                            validity               => iodata()  ,
                            reg_delivery           => integer() ,
                            replace_flag           => integer() },
       OptionalParams :: #{ user_message_reference => integer() ,
                            source_port            => integer() , 
                            source_addr_subunit    => integer() ,
                            destination_port       => integer() ,
                            dest_addr_subunit      => integer() ,
                            sar_msg_ref_num        => integer() ,
                            sar_total_segments     => integer() ,
                            sar_segment_seqnum     => integer() ,
                            more_messages_to_send  => integer() ,
                            payload_type           => integer() ,
                            message_payload        => iodata()  ,
                            privacy_indicator      => integer() ,
                            callback_num           => iodata()  ,
                            callback_num_pres_ind  => integer() ,
                            callback_num_atag      => iodata()  ,
                            source_subaddress      => iodata()  ,
                            dest_subaddress        => iodata()  ,
                            user_response_code     => integer() ,
                            display_time           => integer() ,
                            sms_signal             => integer() ,
                            ms_validity            => integer() ,
                            ms_msg_wait_facilities => integer() ,
                            number_of_messages     => integer() ,
                            alert_on_msg_delivery  => integer() ,
                            language_indicator     => integer() ,
                            its_reply_type         => integer() ,
                            its_session_info       => integer() ,
                            ussd_service_op        => integer() }.
send_sms(C, Sender, Destination, TmpMessage, Options, OptionalParams) ->
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
    service_type    = ServiceType,
    src_addr_ton    = SrcAddrTon,
    src_addr_npi    = SrcAddrNpi,
    src_addr        = Sender,
    dst_addr_ton    = DstAddrTon,
    dst_addr_npi    = DstAddrNpi,
    dst_addr        = Destination,
    data_coding     = DataCoding,
    protocol_id     = ProtocolId,
    priority_flag   = PriorityFlag,
    delivery_time   = DeliveryTime,
    validity        = Validity,
    reg_delivery    = RegDelivery,
    replace_flag    = ReplaceFlag,
    sm_length       = size(Message),
    short_message   = Message,
    optional_params = OptionalParams
  },
  send_sm(C, SubmitSm, Message, DataCoding).

%% @doc Assigns callback to a mobile originating message or delivery receipt
-spec callback(Type, atom(), atom()) -> ok | {error, invalid_type}
  when Type :: message | delivery_receipt.
callback(message, Module, Function) ->
  gen_server:call(callback_mo, Module, Function);
callback(delivery_receipt, Module, Function) ->
  gen_server:call(callback_dr, Module, Function);
callback(_Unknown, _Module, _Function) ->
  {error, invalid_type}.

%% ----------------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------------

%% @private
send_sm(C, Sm, Message, DataCoding) when DataCoding =:= 0, size(Message) =< 160 ->
  [gen_server:call(C, {submit_sm, Sm})];
send_sm(C, Sm, Message, DataCoding) when DataCoding =:= 8, size(Message) =< 140 ->
  [gen_server:call(C, {submit_sm, Sm})];
send_sm(C, Sm, Message, DataCoding) when DataCoding =:= 0 ->
  Ref   = random:uniform(255),
  Size  = size(Message),
  Parts = ceil(Size / 153),
  send_sm(C, Sm, Message, <<>>, Ref, 1, Parts, 153, []);
send_sm(C, Sm, Message, DataCoding) when DataCoding =:= 8 ->
  Ref   = random:uniform(255),
  Size  = size(Message),
  Parts = ceil(Size / 134),
  send_sm(C, Sm, Message, <<>>, Ref, 1, Parts, 134, []).

send_sm(_C, _Sm, <<>>, _Tail, _Ref, _Part, _Parts, _Limit, Acc) ->
  Acc;
send_sm(C, Sm, Str, _Tail, Ref, Part, Parts, Limit, Acc) when size(Str) > Limit ->
  <<BinPart:Limit/binary, BinTail/binary>> = Str,
  send_sm(C, Sm, BinPart, BinTail, Ref, Part, Parts, Limit, Acc);
send_sm(C, Sm, Str, Tail, Ref, Part, Parts, Limit, Acc) ->
  Message = <<5, 0, 3, Ref, Parts, Part, Str/binary>>,
  Length  = size(Message),
  NewSm = Sm#submit_sm_pdu{
    esm_class     = 16#40,
    sm_length     = Length,
    short_message = Message
  },
  NewAcc = Acc ++ [gen_server:call(C, {submit_sm, NewSm})],
  send_sm(C, NewSm, Tail, <<>>, Ref, Part + 1, Parts, Limit, NewAcc).
   

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

%% @private
ceil(X) when X < 0 ->
  trunc(X);
ceil(X) ->
  T = trunc(X),
  case X - T == 0 of
    true  -> T;
    false -> T + 1
  end.
