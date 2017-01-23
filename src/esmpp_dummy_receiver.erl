%% @private This is a default receiver which issues warnings to the console for
%% un-acted MOs and DRs
-module(esmpp_dummy_receiver).

-export([
  mo/3,
  dr/3
]).

%% @private
mo(Sender, Receiver, Message) ->
  io:format(
    standard_error,
    "[~p] Warning - Un-acted message - Sender: ~p - Receiver: ~p - Message: ~p~n", 
    [?MODULE, Sender, Receiver, Message]
  ).

%% @private
dr(Sender, Receiver, Status) ->
  io:format(
    standard_error, 
    "[~p] Warning - Un-acted delivery receipt- Sender: ~p - Receiver: ~p - Status: ~p~n",
    [?MODULE, Sender, Receiver, Status]
  ).

