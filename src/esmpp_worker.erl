%% @private This is the module that manages states and does actual connection
%% to the SMPP server. This has the main logic implementation.
-module(esmpp_worker).

-behaviour(gen_server).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("types.hrl").
-include("commands.hrl").
-include("command_statuses.hrl").

-record(mo_part, {
  max_parts :: integer(),
  src_addr  :: iodata(),
  dst_addr  :: iodata(),
  messages  :: [iodata()]
}).

-record(conn_state, {
  host              :: iodata(),
  port              :: integer(),
  bind_record       :: bind_pdu(),
  reconnect         :: integer(),
  callback_mo       :: {atom(), atom()},
  callback_dr       :: {atom(), atom()}
}).

-record(state, {
  connected   = false :: boolean(), 
  seq_num     =  1    :: integer(),
  binding     =  0    :: integer(),
  status      = -1    :: integer(),
  from_list   = #{}   :: map(),
  concat_list = #{}   :: map(),
  callback_mo         :: {atom(), atom()},
  callback_dr         :: {atom(), atom()},
  socket              :: port(),
  tref                :: timer:tref()
}).

%% ----------------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @private Entry point. Note that another record is used here compared
%% to the rest of the callbacks. This is to minimize footprint as these
%% fields are only used in the lazy initialization
%% ----------------------------------------------------------------------------
init([#{host := Host, port := Port} = Opts,  BindRecord]) ->
  CallbackMO = maps:get(callback_mo, Opts, {esmpp_dummy_receiver, mo}),
  CallbackDR = maps:get(callback_dr, Opts, {esmpp_dummy_receiver, dr}),
  ReconTerm  = maps:get(reconnect, Opts, 1000),
  Reconnect  = get_reconnect(ReconTerm, Host, Port),
  erlang:send_after(Reconnect, self(), connect),
  {ok, #conn_state{
    host        = Host,
    port        = Port,
    reconnect   = Reconnect,
    bind_record = BindRecord,
    callback_mo = CallbackMO,
    callback_dr = CallbackDR
  }}.

%% ----------------------------------------------------------------------------
%% @private Checks if the process is currently connected to the SMSC 
%% ----------------------------------------------------------------------------
handle_call(is_connected, _From, #state{connected=Connected} = State) ->
  {reply, {status, Connected}, State}; 

%% ----------------------------------------------------------------------------
%% @private Default catcher for the alternative connecting state
%% ----------------------------------------------------------------------------
handle_call(is_connected, _From, #conn_state{} = State) ->
  {reply, {status, false}, State}; 

%% ----------------------------------------------------------------------------
%% @private Assigns callback module and function for MOs
%% ----------------------------------------------------------------------------
handle_call({callback_mo, Module, Function}, _From, State) ->
  {reply, ok, State#state{
    callback_mo = {Module, Function}
  }}; 

%% ----------------------------------------------------------------------------
%% @private Assigns callback module and function for DRs
%% ----------------------------------------------------------------------------
handle_call({callback_dr, Module, Function}, _From, State) ->
  {reply, ok, State#state{
    callback_dr = {Module, Function}
  }}; 

%% ----------------------------------------------------------------------------
%% @private submit_sm attempt when not connected
%% ----------------------------------------------------------------------------
handle_call({submit_sm, _SubmitSm}, _From,
             #state{connected=false} = State) ->
  {reply, {error, not_connected}, State}; 

%% ----------------------------------------------------------------------------
%% @private submit_sm attempt when bound as receiver
%% ----------------------------------------------------------------------------
handle_call({submit_sm, _SubmitSm}, _From,
             #state{binding=?BIND_RECEIVER} = State) ->
  {reply, {error, not_allowed}, State}; 

%% ----------------------------------------------------------------------------
%% @private submit_sm packet sending
%% ----------------------------------------------------------------------------
handle_call({submit_sm, SubmitSm}, From, 
             #state{socket=Socket, seq_num=Seq, from_list=Clients} = State) ->
  NewSeq = increment(Seq),
  {pdu, Packet} = esmpp_pdu:submit_sm(NewSeq, SubmitSm), 
  send(Socket, Packet),
  NewClients = maps:put(NewSeq, From, Clients),
  {noreply, State#state{
    from_list = NewClients,
    seq_num   = NewSeq
  }}; 

%% ----------------------------------------------------------------------------
%% @private Catch all
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% ----------------------------------------------------------------------------
%% @private unbind packet sending
%% ----------------------------------------------------------------------------  
handle_cast(unbind, #state{socket=Socket, seq_num=Seq} = State) ->
  NewSeq = increment(Seq),
  {pdu, Packet} = esmpp_pdu:unbind(NewSeq),
  send(Socket, Packet), 
  {noreply, State#state{
    seq_num = NewSeq
  }};

%% @private
handle_cast(_Message, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @private Does the actual connection to the remote SMSC
%% ----------------------------------------------------------------------------
handle_info(connect, #conn_state{host=Host, port=Port,
                                 callback_mo=CallbackMO,
                                 callback_dr=CallbackDR, 
                                 bind_record=BindRecord}) ->
  {pdu,    Packet} = esmpp_pdu:bind(1, BindRecord),
  {socket, Socket} = get_socket(Host, Port),
  send(Socket, Packet),
  {noreply, #state{
    seq_num     = 1,
    socket      = Socket,
    callback_mo = CallbackMO,
    callback_dr = CallbackDR
  }};

%% ----------------------------------------------------------------------------
%% @private generic_nack receiver
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?GENERIC_NACK:32, Status:32,
                             _Seq:32, _Data/binary>>} = PDU, State) ->
  StatusAtom = esmpp_status:to_atom(Status),
  io:format(standard_error, "[generic_nack] ~p: ~p", [StatusAtom, PDU]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Successfully binded as receiver
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?BIND_RECEIVER_RESP:32, ?ESME_ROK:32, 
                            _Seq:32, _Data/binary>>}, State) ->
  {ok, TRef}  = keepalive(),
  {noreply, State#state{
    connected = true,
    binding   = ?BIND_RECEIVER,
    status    = ?ESME_ROK,
    socket    = Socket,
    tref      = TRef
  }};

%% ----------------------------------------------------------------------------
%% @private Error binding as receiver
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?BIND_RECEIVER_RESP:32, Status:32, 
                             _Seq:32, _Data/binary>>}, State) ->
  
  StatusAtom = esmpp_status:to_atom(Status),
  io:format(standard_error, "[bind_receiver_resp] ~p", [StatusAtom]),
  {noreply, State#state{
    status    = Status
  }};

%% ----------------------------------------------------------------------------
%% @private Successfully binded as transmitter
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?BIND_TRANSMITTER_RESP:32, ?ESME_ROK:32, 
                            _Seq:32, _Data/binary>>}, State) ->
  {ok, TRef}  = keepalive(),
  {noreply, State#state{
    connected = true,
    binding   = ?BIND_TRANSMITTER,
    status    = ?ESME_ROK,
    socket    = Socket,
    tref      = TRef
  }};

%% ----------------------------------------------------------------------------
%% @private Error binding as transmitter
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?BIND_TRANSMITTER_RESP:32, Status:32, 
                             _Seq:32, _Data/binary>>}, State) ->
  
  StatusAtom = esmpp_status:to_atom(Status),
  io:format(standard_error, "[bind_transmitter_resp] ~p", [StatusAtom]),
  {noreply, State#state{
    status    = Status
  }};

%% ----------------------------------------------------------------------------
%% @private Successfully binded as transceiver 
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?BIND_TRANSCEIVER_RESP:32, ?ESME_ROK:32,
                            _Seq:32, _Data/binary>>}, State) ->
  {ok, TRef}  = keepalive(),
  {noreply, State#state{
    connected = true,
    binding   = ?BIND_TRANSCEIVER,
    status    = ?ESME_ROK,
    socket    = Socket,
    tref      = TRef
  }};

%% ----------------------------------------------------------------------------
%% @private Error binding as transceiver
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?BIND_TRANSCEIVER_RESP:32, Status:32, 
                             _Seq:32, _Data/binary>>}, State) ->
  
  StatusAtom = esmpp_status:to_atom(Status),
  io:format(standard_error, "[bind_transceiver_resp] ~p", [StatusAtom]),
  {noreply, State#state{
    status    = Status
  }};

%% ----------------------------------------------------------------------------
%% @private Successfully sent a submit_sm request
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?SUBMIT_SM_RESP:32, ?ESME_ROK:32,
                              Seq:32, MessageId/binary>>}, 
                             #state{from_list=Clients} = State) ->
  Client = maps:get(Seq, Clients, '__undefined__'),
  gen_server:reply(Client, {message_id, strip_null(MessageId)}),
  {noreply, State#state{
    from_list = maps:remove(Seq, Clients)
  }};

%% ----------------------------------------------------------------------------
%% @private Unsuccessful submit_sm request
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?SUBMIT_SM_RESP:32, Status:32,
                              Seq:32, _Data/binary>>}, 
                             #state{from_list=Clients} = State) ->
  Client = maps:get(Seq, Clients, '__undefined__'),
  gen_server:reply(Client, {error, esmpp_status:to_atom(Status)}),
  {noreply, State#state{
    from_list = maps:remove(Seq, Clients)
  }};

%% ----------------------------------------------------------------------------
%% @private deliver_sm receiver 
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?DELIVER_SM:32, ?ESME_ROK:32, Seq:32,
                            Data/binary>>}, State) ->
  {cstring, _SvcType, Data2}      = get_cstring(Data),
  <<_SrcTonNpi:16, Data3/binary>> = Data2,
  {cstring, SrcAddr, Data4}       = get_cstring(Data3),
  <<_DstTonNpi:16, Data5/binary>> = Data4,
  {cstring, DstAddr, Data6}       = get_cstring(Data5),
  <<EsmClass:8, Data7/binary>>    = Data6,
  <<_Misc1:48, Data8/binary>>     = Data7,
  <<DataCoding:8, Data9/binary>>  = Data8,
  <<_Misc2:8, Data10/binary>>     = Data9,
  <<Length:8, Data11/binary>>     = Data10,
  <<Message:Length/binary, _Data12/binary>> = Data11,
  FinalMessage  = normalize_encoding(DataCoding, Message),
  {pdu, Packet} = esmpp_pdu:deliver_sm_resp(Seq),
  send(Socket, Packet),
  erlang:send(self(), {callback, EsmClass, SrcAddr, DstAddr, FinalMessage}),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Chopped MO event (complete)
%% ----------------------------------------------------------------------------

handle_info({chopped_mo, Key, #mo_part{max_parts=MaxParts, src_addr=SrcAddr,
                                       dst_addr=DstAddr, messages=Messages}},
                                #state{concat_list=ConcatMap} = State)
                                  when length(Messages) >= MaxParts ->
  SortedMessages = lists:keysort(1, Messages),
  Message = lists:foldl(fun({_Part, MsgPart}, Acc) -> 
    <<Acc/binary, MsgPart/binary>> 
  end, <<>>, SortedMessages),
  erlang:send(self(), {callback, 0, SrcAddr, DstAddr, Message}),
  NewConcatMap = maps:remove(Key, ConcatMap),
  {noreply, State#state{
    concat_list = NewConcatMap
  }};

%% ----------------------------------------------------------------------------
%% @private Chopped MO event (incomplete)
%% ----------------------------------------------------------------------------

handle_info({chopped_mo, _Key, _MOPart}, State) ->
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Callback from deliver_sm - Chopped MO
%% ----------------------------------------------------------------------------

handle_info({callback, 64, SrcAddr, DstAddr, <<UDH:6/binary, MsgPart/binary>>}, 
                                     #state{concat_list=ConcatMap} = State) ->
  <<_Misc:24, Ref:8, Parts:8, Part:8>> = UDH,
  RefBin       = integer_to_binary(Ref),
  KeyBin       = <<SrcAddr/binary, ":", DstAddr/binary, ":", RefBin/binary>>,
  MOPart       = #mo_part{
    max_parts  = Parts,
    src_addr   = SrcAddr,
    dst_addr   = DstAddr,
    messages   = []
  },
  CurrMOPart   = maps:get(KeyBin, ConcatMap, MOPart),
  CurrPairs    = CurrMOPart#mo_part.messages,
  PartPair     = {Part, MsgPart},
  NewMOPart    = CurrMOPart#mo_part{
    messages   = CurrPairs ++ [PartPair]
  },
  NewConcatMap = maps:put(KeyBin, NewMOPart, ConcatMap),
  erlang:send(self(), {chopped_mo, KeyBin, NewMOPart}),
  {noreply, State#state{
    concat_list = NewConcatMap
  }};

%% ----------------------------------------------------------------------------
%% @private Successful UNBIND_RESP
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?UNBIND_RESP:32,  ?ESME_ROK:32,
                             _Seq:32, _Data/binary>>}, State) ->
  {noreply, State#state{
    connected = false,
    seq_num = 0
  }};

%% ----------------------------------------------------------------------------
%% @private Callback from deliver_sm - Generic MO
%% ----------------------------------------------------------------------------

handle_info({callback, 0, SrcAddr, DstAddr, Message}, 
                                     #state{callback_mo={Mod, Fun}} = State) ->
  spawn(Mod, Fun, [SrcAddr, DstAddr, Message]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Callback from deliver_sm - Generic DR
%% ----------------------------------------------------------------------------

handle_info({callback, 4, SrcAddr, DstAddr, Message}, 
                                     #state{callback_dr={Mod, Fun}} = State) ->
  DeliveryReceipt = dr_to_map(Message),
  spawn(Mod, Fun, [smpp, SrcAddr, DstAddr, DeliveryReceipt]),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Handle enquire_link after unbind
%% ----------------------------------------------------------------------------
handle_info(enquire_link, #state{connected=false} = State) ->
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Keep-alive (enquire_link)
%% ----------------------------------------------------------------------------
handle_info(enquire_link, #state{socket=Socket, seq_num=SeqNum} = State) ->
  NewSeqNum        = SeqNum + 1,
  {pdu,    Packet} = esmpp_pdu:enquire_link(NewSeqNum),
  send(Socket, Packet),
  {noreply, State#state{
    seq_num = NewSeqNum
  }};

%% ----------------------------------------------------------------------------
%% @private Successful enquire_link 
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?ENQUIRE_LINK_RESP:32, ?ESME_ROK:32,
                             _Seq:32, _Data/binary>>}, State) ->
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Connection closed from unbind
%% ----------------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, #state{connected=false, tref=TRef} = State) ->
  timer:cancel(TRef),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Connection closed
%% ----------------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
  exit(disconnected),
  {noreply, State#state{
    connected = false,
    status    = -1,
    binding   = 0,
    seq_num   = 0
  }};

%% ----------------------------------------------------------------------------
%% @private Catch all
%% ----------------------------------------------------------------------------
handle_info(_Info, State) ->
  io:format(standard_error, "[~p] Warning Unknown Message: ~p", [
    ?MODULE, _Info
  ]),
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ----------------------------------------------------------------------------
%% internal
%% ----------------------------------------------------------------------------

%% @private
normalize_encoding(8, Message) ->
  unicode:characters_to_binary(Message, utf16, utf8);
normalize_encoding(_DataCoding, Message) ->
  Message.

%% @private DR text to Erlang map
dr_to_map(Bin) ->
  Bin1  = binary:replace(Bin, <<"submit date">>, <<"submit_date">>),
  Bin2  = binary:replace(Bin1, <<"done date">>, <<"done_date">>),
  Parts = binary:split(Bin2, <<" ">>, [global]),
  lists:foldl(fun(KeyPair, Acc) ->
    build_map(binary:split(KeyPair, <<":">>), Acc)
  end, #{}, Parts).

%% @private Builds actual DR map
build_map([Key, Val], Acc) ->
  maps:put(binary_to_atom(Key, utf8), Val, Acc);
build_map([Val], Acc) ->
  TmpText = maps:get(text, Acc, <<>>),
  maps:put(text, <<TmpText/binary, " ", Val/binary>>, Acc).

%% @private 
keepalive() ->
  timer:send_interval(20000, enquire_link). 

%% @private
get_socket(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  {socket, Socket}.

%% @private
send(Socket, Packet) ->
  ok = gen_tcp:send(Socket, Packet).

%% @private
increment(Seq) ->
  Seq + 1.

%% @private
strip_null(Bin) ->
  Limit = size(Bin) - 1,
  <<BinPart:Limit/binary, _Null>> = Bin,
  BinPart.

%% @private
get_cstring(<<>>) ->
  {error, empty_bin};
get_cstring(Bin) ->
  get_cstring([B || <<B:1/binary>> <= Bin], <<>>).

%% @private
get_cstring([<<0>> | Tail], AccBin) ->
  {cstring, AccBin, bin_combine(Tail)};
get_cstring([B | Tail], AccBin) ->
  get_cstring(Tail, <<AccBin/binary, B/binary>>).

%% @private
bin_combine(BinList) ->
  lists:foldl(fun(B, AccBin) ->
    <<AccBin/binary, B/binary>>
  end, <<>>, BinList).

%% @private
get_reconnect({Module, Function}, Host, Port) ->
  HostBin = esmpp_format:ensure_binary(Host),
  PortBin = esmpp_format:ensure_binary(Port),
  Id      = <<"id_", HostBin/binary, ":", PortBin/binary>>,
  apply(Module, Function, [Id]);
get_reconnect(Time, _Host, _Port) when is_integer(Time) ->
  Time;
get_reconnect(_Unknown, _Host, _Port) ->
  1000.
