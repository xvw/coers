%% @author X. Van de Woestyne <xaviervdw@gmail.com>
%% @copyright 2016 X. Van de Woestyne
%% @version 0.1.0
%% @title Coers, a small coersion library for Erlang
%% @doc `coers` provide small function for value coersion.

-module(coers).
-export([
  new/2,
  fail/1,
  value/1
]).

%% Results of coersion are wrapped into a result record
-record(result, {
  succeeded :: boolean(),
  value     :: term()
}).
-type result() :: #result{}.

%% @doc Create a new result
-spec new(boolean(), term()) -> result().
new(Flag, Value) ->
  #result{
    succeeded = Flag,
    value = Value
  }.

%% @doc determine if a coersion is succeeded
-spec fail(result()) -> boolean().
fail(Coersion) ->
  not (Coersion#result.succeeded).

%% @doc extract the value of a wrapped result
-spec value(result()) -> term().
value(Coersion) ->
  Coersion#result.value.
