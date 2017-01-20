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

-record(conn_state, {
  host              :: iodata(),
  port              :: integer(),
  bind_record       :: bind_pdu()
}).

-record(state, {
  connected = false :: boolean(), 
  seq_num   =  1    :: integer(),
  binding   =  0    :: integer(),
  status    = -1    :: integer(),
  from_list = #{}   :: map(),
  callback_mo       :: {atom(), atom()},
  callback_dr       :: {atom(), atom()},
  socket            :: port(),
  tref              :: {integer(), reference()}
}).

%% ----------------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @private Entry point. Note that another record is used here compared
%% to the rest of the callbacks. This is to minimize footprint as these
%% fields are only used in the lazy initialization
%% ----------------------------------------------------------------------------
init([#{host := Host, port := Port},  BindRecord]) ->
  {ok, #conn_state{
    host        = Host,
    port        = Port,
    bind_record = BindRecord
  }, 0}.

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

%% @private
handle_cast(_Message, State) ->
  {noreply, State}.

%% ----------------------------------------------------------------------------
%% @private This does the actual connection. Sleep for 3s, then attempt
%% to connect. This is a lazy initiation which gives the erlang supervisor
%% enough time to wait for the connection to resume indefinitely
%% ----------------------------------------------------------------------------
handle_info(timeout, #conn_state{host=Host, port=Port, 
                                 bind_record=BindRecord}) ->
  timer:sleep(1000),
  {pdu,    Packet} = esmpp_pdu:bind(1, BindRecord),
  {socket, Socket} = get_socket(Host, Port),
  send(Socket, Packet),
  {noreply, #state{
    seq_num   = 1,
    socket    = Socket
  }};

%% ----------------------------------------------------------------------------
%% @private generic_nack receiver
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?GENERIC_NACK:32, Status:32,
                             _Seq:32, _Data/binary>>} = PDU, State) ->
  StatusAtom = esmpp_status:to_atom(Status),
  io:format("[generic_nack] ~p: ~p", [StatusAtom, PDU]),
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
handle_info({tcp, _Socket, <<_Len:32, ?DELIVER_SM:32, ?ESME_ROK:32,
                             _Seq:32, _Data/binary>>}, State) ->
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
%% @private Connection closed
%% ----------------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, #state{tref=TRef} = State) ->
  {ok, cancel} = timer:cancel(TRef),
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
  io:format("Warning Unknown Message: ~p", [_Info]),
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
