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
  start_link/1
]).

%% @doc Starts the connection to the SMPP server and does the binding process.
%%
%% `Options` is a map that should contain the following keys:
%%
%% <dt>`name`</dt>
%% <dd>If provided, the internal gen_server will be registered with this name.
%%     see gen_server:start_link/4</dd>
%% <dt>`host`</dt>
%% <dd>Hostname or IP address of the SMPP server</dd>
%% <dt>`port`</dt>
%% <dd>Port of the SMPP server</dd>
%% <dt>`system_id`</dt>
%% <dd>Identifier for the SMPP client</dd>
%% <dt>`password`</dt>
%% <dd>Password used to authenticate with the system_id</dd>
%% <dt>`system_type`</dt>
%% <dd>Identifier for the type of SMPP Client</dd>
%% <dt>`addr_ton`</dt>
%% <dd>Type of number for the SMPP server address. if unknown, can be unset</dd>
%% <dt>`addr_npi`</dt>
%% <dd>Number planning indicator for the SMPP server address. If unknown, can be unset</dd>
%% <dt>`address_range`</dt>
%% <dd>The SMPP server address. If unknown, can be unset</dd>
-spec start_link(Options) -> {ok, pid()} | ignore | {error, term()} 
  when Options    :: #{ name          => ServerName ,
                        host          => iodata()   ,
                        port          => integer()  ,
                        system_id     => iodata()   ,
                        password      => iodata()   ,
                        system_type   => iodata()   ,
                        addr_ton      => integer()  ,
                        addr_npi      => integer()  ,
                        address_range => iodata()   },
       ServerName :: {local,  Name       :: atom()} |
                     {global, GlobalName :: term()} |
                     {via,    Module     :: atom(), ViaName :: term()}.
start_link(Options) ->
  case maps:get(name, Options, '__undefined__') of
    '__undefined__' ->
      gen_server:start_link(esmpp_worker, Options, []);
    Name ->
      gen_server:start_link(Name, esmpp_worker, Options, [])
  end.


