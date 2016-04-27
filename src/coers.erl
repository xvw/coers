%% @author X. Van de Woestyne <xaviervdw@gmail.com>
%% @copyright 2016 X. Van de Woestyne
%% @version 0.1.0
%% @title Coers, a small coersion library for Erlang
%% @doc `coers` provide small function for value coersion.

-module(coers).
-vsn(1).
-author(["Xavier van De Woestyne"]).

-export([
  new/2,
  map/2,
  fmap/2,
  succeed/1,
  fail/1,
  value/1,
  is_ascii_char/1,
  maybe_string/1,
  to_string/1,
  of_string/1
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

%% @doc Apply a function to a result
-spec map(fun(), result()) -> term().
map(F, Result) ->
  F(value(Result)).

%% @doc Apply and wrap a function to a result
-spec fmap(fun(), result()) -> result().
fmap(F, Result) ->
  new(succeed(Result), map(F, Result)).

%% @doc determine if a coersion is a success
-spec succeed(result()) -> boolean().
succeed(Coersion) ->
  Coersion#result.succeeded.

%% @doc determine if a coersion is a failure
-spec fail(result()) -> boolean().
fail(Coersion) ->
  not succeed(Coersion).

%% @doc extract the value of a wrapped result
-spec value(result()) -> term().
value(Coersion) ->
  Coersion#result.value.

%% @doc determine if an integer is a potential Ascii Char
-spec is_ascii_char(integer()) -> boolean().
is_ascii_char(X) when is_integer(X) ->
  (X >= 32) and (X < 127);
is_ascii_char([H]) ->
  is_ascii_char(H);
is_ascii_char(_) ->
  false.

%% @doc check if a list is maybe a string
-spec maybe_string(list()) -> boolean().
maybe_string(List) when is_list(List) ->
  lists:all(fun is_ascii_char/1, List);
maybe_string(_) -> false.

%% @doc try to coers term into string
-spec to_string(term()) -> string().
to_string(Term) when is_bitstring(Term) ->
  List = binary_to_list(Term),
  to_string(List);
to_string(Term) ->
  case maybe_string(Term) of
    true -> new(true, Term);
    false ->
      List = io_lib:format("~p", [Term]),
      new(true, lists:flatten(List))
    end.

%% @doc an ugly and magic coersion from string to term()
%% @doc this function not should be used ...
-spec of_string(string()) -> result().
of_string(String) ->
  {ok, Regexp} = re:compile("^.+(\\,|\\;|\\.)$"),
    S =
      case re:run(String, Regexp) of
        {match, [_, {Offset, _}]} ->
          Substring = string:substr(String, 1, Offset -1),
          Substring ++ ".";
        _ -> String ++ "."
      end,
    case erl_scan:string(S) of
      {ok, Tokens, _} ->
        case erl_parse:parse_exprs(Tokens) of
          {ok, Exprs} ->
            {value, Result, []} = erl_eval:exprs(Exprs, []),
            new(true, Result);
          {error, {_, _, _}} ->
            new(false, none)
        end;
      {error, {_, _, _}, _} ->
        new(false, none)
  end.
