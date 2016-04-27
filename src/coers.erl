%% @author X. Van de Woestyne <xaviervdw@gmail.com>
%% @copyright 2016 X. Van de Woestyne
%% @version 0.1.0
%% @doc coers provide small function for value coersion.

-module(coers).
-vsn(1).
-author(["Xavier van De Woestyne"]).

%% API of Coers
-export([
  new/2,
  map/2,
  fmap/2,
  unless/2,
  traverse/1,
  succeed/1,
  fail/1,
  value/1,
  is_ascii_char/1,
  maybe_string/1,
  to_string/1,
  to_string/2,
  of_string/1,
  of_string/2,
  to_int/1,
  to_int/2,
  to_float/1,
  to_float/2,
  to_atom/1,
  to_atom/2,
  to_bool/1,
  to_bool/2
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

%% @doc Unabstract result as a tuple
-spec traverse(result()) -> {boolean(), term()}.
traverse(Result) ->
  {succeed(Result), value(Result)}.

%% @doc Apply a function to a result
-spec map(fun(), result()) -> term().
map(F, Result) ->
  F(value(Result)).

%% @doc Apply and wrap a function to a result
-spec fmap(fun(), result()) -> result().
fmap(F, Result) ->
  new(succeed(Result), map(F, Result)).

%% @doc Replace value if coersion failed the suceeded flag is preserved
-spec unless(result(), term()) -> result().
unless(Result, Default) ->
  case succeed(Result) of
    true  -> Result;
    false -> fmap(fun(_) -> Default end, Result)
  end.

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

%% @doc Replace value if coersion failed
%% @doc the suceeded flag is preserved
-spec to_string(term(), term()) -> result().
to_string(Term, Default) ->
  unless(to_string(Term), Default).

%% @doc an ugly and magic coersion from string to term()
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

%% @doc try coersion or define a default value the suceeded flag is preserved
-spec of_string(string(), term()) -> result().
of_string(Str, Default) ->
  unless(of_string(Str), Default).

%% @doc numeric alignement of a string (float of int)
-spec numeric_align(string()) -> atom().
numeric_align(String) ->
  {ok, Regexp} = re:compile("^\\d+(\\.|\\,)?"),
    case re:run(String, Regexp) of
      {match, [_A]} -> integer;
      {match, [_A,_B]} -> float;
    _ -> any
  end.

%% @doc try to coers a term to an integer
-spec to_int(term()) -> result().
to_int(Obj) when is_integer(Obj) -> new(true, Obj);
to_int(Obj) when is_float(Obj)   -> new(true, round(Obj));
to_int(Obj) when is_bitstring(Obj) -> to_int(binary_to_list(Obj));
to_int(Obj) when is_list(Obj)    ->
  try list_to_integer(Obj) of
  Result    -> new(true, Result)
  catch _:_ ->
    case numeric_align(Obj) of
      float -> to_int(list_to_float(Obj));
      _     -> new(false, 0)
    end
  end;
to_int(Obj) when is_atom(Obj)     ->
  try Soft = atom_to_list(Obj), to_int(Soft) of
  Result     -> Result
  catch  _:_ ->
    new(false, 0)
  end;
to_int(_) -> new(false, 0).

%% @doc try coersion or define a default value
%% @doc the suceeded flag is preserved
-spec to_int(term(), term()) -> result().
to_int(Term, Default) ->
  unless(to_int(Term), Default).

%% @doc try to coers a term to a float
-spec to_float(term()) -> result().
to_float(Obj) when is_float(Obj)     -> new(true, Obj);
to_float(Obj) when is_integer(Obj)   -> new(true, float(Obj));
to_float(Obj) when is_bitstring(Obj) -> to_float(binary_to_list(Obj));
to_float(Obj) when is_list(Obj)      ->
  try list_to_float(Obj) of
  Result     -> new(true, Result)
  catch  _:_ ->
    case numeric_align(Obj) of
      integer -> to_float(list_to_integer(Obj));
      _       -> new(false, 0.0)
    end
  end;
to_float(Obj) when is_atom(Obj)      ->
  try Pred = atom_to_list(Obj), to_float(Pred) of
  Result -> Result
  catch _:_ ->
    new(false, 0.0)
  end;
to_float(_) -> new(false, 0.0).

%% @doc try coersion or define a default value the suceeded flag is preserved
-spec to_float(term(), term()) -> result().
to_float(Term, Default) ->
  unless(to_float(Term), Default).

%% @doc try to coers a term to an atom
-spec to_atom(term()) -> result().
to_atom(Obj) when is_atom(Obj)  -> new(true, Obj);
to_atom(Obj) when is_list(Obj)  ->
  try list_to_atom(Obj) of
  Result    -> new(true, Result)
  catch _:_ -> new(false, false)
  end;
to_atom(Obj) ->
  Pred = to_string(Obj),
  to_atom(value(Pred)).

%% @doc try coersion or define a default value the suceeded flag is preserved
-spec to_atom(term(), term()) -> result().
to_atom(Term, Default) ->
  unless(to_atom(Term), Default).

%% @doc try to coers a term to a boolean
-spec to_bool(term()) -> boolean().
to_bool(Obj) when is_atom(Obj) ->
  new((Obj == true) or (Obj == false), not (Obj == false));
to_bool(Obj) when is_list(Obj) ->
  case string:to_lower(Obj) of
    "true"   -> new(true, true);
    "false"  -> new(true, false);
    _        -> new(false, true)
  end;
to_bool(X) when is_bitstring(X) ->
  Pred = to_string(X),
  to_bool(value(Pred));
to_bool(0)   -> new(true, false);
to_bool(0.0) -> new(true, false);
to_bool(1)   -> new(true, true);
to_bool(1.0) -> new(true, true);
to_bool(_)   -> new(false, true).

%% @doc try coersion or define a default value the suceeded flag is preserved
-spec to_bool(term(), term()) -> result().
to_bool(Term, Default) ->
  unless(to_bool(Term), Default).
