%% @private
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

-record(state, {
  connected = false :: boolean(), 
  seq_num   =  1    :: integer(),
  binding   =  0    :: integer(),
  status    = -1    :: integer(),
  socket            :: port(),
  from              :: {pid(), term()}
}).

%% ----------------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @private submit_sm attempt when not connected
%% ----------------------------------------------------------------------------
init([#{host := Host, port := Port},  BindRecord]) ->
  {pdu,    Packet} = esmpp_pdu:bind(1, BindRecord),
  {socket, Socket} = get_socket(Host, Port),
  send(Socket, Packet),
  {ok, #state{
    seq_num   = 1,
    socket    = Socket
  }}.

%% ----------------------------------------------------------------------------
%% @private submit_sm attempt when not connected
%% ----------------------------------------------------------------------------
handle_call({submit_sm, _SubmitSm, _Options}, _From, #state{connected=false} = State) ->
  {reply, {error, not_connected}, State}; 

%% ----------------------------------------------------------------------------
%% @private submit_sm packet sending
%% ----------------------------------------------------------------------------
handle_call({submit_sm, SubmitSm, _Options}, From, #state{socket=Socket, seq_num=SeqNum} = State) ->
  NewSeqNum = SeqNum + 1,
  {pdu, Packet} = esmpp_pdu:submit_sm(NewSeqNum, SubmitSm), 
  send(Socket, Packet),
  {noreply, State#state{
    from    = From,
    seq_num = NewSeqNum
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
%% @private Successfully binded as receiver
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?BIND_RECEIVER_RESP:32, ?ESME_ROK:32, _Seq:32, _Data/binary>>}, State) ->
  {noreply, State#state{
    connected = true,
    binding   = ?BIND_RECEIVER,
    status    = ?ESME_ROK,
    socket    = Socket
  }};

%% ----------------------------------------------------------------------------
%% @private Successfully binded as transmitter
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?BIND_TRANSMITTER_RESP:32, ?ESME_ROK:32, _Seq:32, _Data/binary>>}, State) ->
  {noreply, State#state{
    connected = true,
    binding   = ?BIND_TRANSMITTER,
    status    = ?ESME_ROK,
    socket    = Socket
  }};

%% ----------------------------------------------------------------------------
%% @private Successfully binded as tranceiver 
%% ----------------------------------------------------------------------------
handle_info({tcp, Socket, <<_Len:32, ?BIND_TRANSCEIVER_RESP:32, ?ESME_ROK:32, _Seq:32, _Data/binary>>}, State) ->
  {noreply, State#state{
    connected = true,
    binding   = ?BIND_TRANSCEIVER,
    status    = ?ESME_ROK,
    socket    = Socket
  }};

%% ----------------------------------------------------------------------------
%% @private Successfully sent a submit_sm request
%% ----------------------------------------------------------------------------
handle_info({tcp, _Socket, <<_Len:32, ?SUBMIT_SM_RESP:32, ?ESME_ROK:32, _Seq:32, _Data/binary>>}, #state{from=Client} = State) ->
  gen_server:reply(Client, ok),
  {noreply, State};

%% ----------------------------------------------------------------------------
%% @private Connection closed
%% TODO: Reconnection
%% ----------------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
  {noreply, State#state{
    connected = false,
    binding   = 0
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
get_socket(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  {socket, Socket}.

%% @private
send(Socket, Packet) ->
  ok = gen_tcp:send(Socket, Packet).
