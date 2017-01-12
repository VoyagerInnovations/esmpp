%% @doc Generic formatting helper
-module(esmpp_format).

-export([
  ensure_binary/1
]).

%% @doc Ensures that the term passed to this function is a binary.
%% If the variable passed contains list, int, or atom, it will be
%% converted to binary.
-spec ensure_binary(iodata() | integer() | atom()) -> binary().
ensure_binary(Term) when is_binary(Term) ->
  Term;
ensure_binary(Term) when is_integer(Term) ->
  integer_to_binary(Term);
ensure_binary(Term) when is_list(Term) ->
  list_to_binary(Term);
ensure_binary(Term) when is_atom(Term) ->
  atom_to_binary(Term, utf8).
