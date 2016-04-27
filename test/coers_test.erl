%% @author X. Van de Woestyne <xaviervdw@gmail.com>
%% @copyright 2016 X. Van de Woestyne
%% @version 0.1.0
%% @title Coers, a small coersion library for Erlang
%% @doc `coers` provide small function for value coersion.

-module(coers_test).
-vsn(1).
-author(["Xavier van De Woestyne"]).
-include_lib("eunit/include/eunit.hrl").

%% Test for coers:succeed
succeed_test() ->
  MockFailure = coers:new(false, 0),
  ?assertNot(coers:succeed(MockFailure)),
  MockSuccess = coers:new(true, 0),
  ?assert(coers:succeed(MockSuccess)).


%% Test for coers:fail
fail_test() ->
  MockFailure = coers:new(false, 0),
  ?assert(coers:fail(MockFailure)),
  MockSuccess = coers:new(true, 0),
  ?assertNot(coers:fail(MockSuccess)).

%% Test for coers:value
value_test() ->
  MockOne = coers:new(true, 1),
  ?assertEqual(coers:value(MockOne), 1),
  MockAtom = coers:new(true, hello),
  ?assertEqual(coers:value(MockAtom), hello),
  MockFloat = coers:new(true, 1.12),
  ?assertEqual(coers:value(MockFloat), 1.12),
  MockStr = coers:new(true, "foo"),
  ?assertEqual(coers:value(MockStr), "foo").

%% Test for coers:is_ascii_char
is_ascii_char_test() ->
  Flag = lists:all(
    fun coers:is_ascii_char/1,
    lists:seq(32, 126)
  ),
  ?assert(Flag),
  ?assert(coers:is_ascii_char("A")),
  ?assertNot(coers:is_ascii_char(222)),
  ?assertNot(coers:is_ascii_char(a)).

%% Test for maybe_list
maybe_list_test() ->
  ?assert(coers:maybe_string("")),
  ?assert(coers:maybe_string("Hello")),
  ?assert(coers:maybe_string([32, 33, 34])),
  ?assertNot(coers:maybe_string(45)),
  ?assertNot(coers:maybe_string([0,1])),
  ?assertNot(coers:maybe_string(atom)),
  ?assertNot(coers:maybe_string(45.0)).


%% Test for to_string coersion
to_string_test() ->
  ToStr = fun(X) ->
    coers:map(
      fun(Y) -> Y end,
      coers:to_string(X)
    )
  end,
  ?assertEqual(ToStr("coers"), "coers"),
  ?assertEqual(ToStr(coers),"coers"),
  ?assertEqual(ToStr([]),""),
  ?assertEqual(ToStr(45), "45"),
  ?assertEqual(ToStr(<<"coers">>), "coers"),
  ?assertEqual(ToStr(45.0), "45.0").

%% Test suits for magic coersion
of_string_atomic_test() ->
  R = coers:of_string("an_atom"),
  ?assert(coers:succeed(R)),
  ?assertEqual(coers:value(R), an_atom).

of_string_list_test() ->
  R = coers:of_string("[1,2,3,4]"),
  ?assert(coers:succeed(R)),
  ?assertEqual(coers:value(R), [1,2,3,4]).

of_string_numeric_test() ->
  R = coers:of_string("{45, 45.3}"),
  ?assert(coers:succeed(R)),
  ?assertEqual(coers:value(R), {45, 45.3}).

of_string_bitstring_test() ->
  R = coers:of_string("<<\"foo\">>"),
  ?assert(coers:succeed(R)),
  ?assertEqual(coers:value(R), <<"foo">>).

of_string_error_test() ->
  R = coers:of_string("{45"),
  ?assert(coers:fail(R)).

unless_test() ->
  R = coers:new(true, 45),
  Rp = coers:unless(R, 12),
  ?assertEqual(coers:value(Rp), 45).

unless2_test() ->
  R = coers:new(false, 45),
  Rp = coers:unless(R, 12),
  ?assertEqual(coers:value(Rp), 12).

of_string_defensive_test() ->
  R = coers:of_string("<<43", <<"43">>),
  U = coers:of_string("foo", bar),
  ?assertEqual(coers:value(R), <<"43">>),
  ?assertEqual(coers:value(U), foo).

to_int_test() ->
  F = fun(X) -> coers:value(coers:to_int(X)) end,
  G = fun(X, Y) -> coers:value(coers:to_int(X, Y)) end,
  ?assertEqual(F("1000"), 1000),
  ?assertEqual(F(444), 444),
  ?assertEqual(F('123'), 123),
  ?assertEqual(F(111.2), 111),
  ?assertEqual(F("+0123"), 123),
  ?assertEqual(F(<<"23">>), 23),
  ?assertEqual(G("foo", 111), 111).

to_float_test() ->
  F = fun(X) -> coers:value(coers:to_float(X)) end,
  G = fun(X, Y) -> coers:value(coers:to_float(X, Y)) end,
  ?assertEqual(F("1000"), 1000.0),
  ?assertEqual(F(444), 444.0),
  ?assertEqual(F('123.23'), 123.23),
  ?assertEqual(F(111.2), 111.2),
  ?assertEqual(F("+0123.7654"), 123.7654),
  ?assertEqual(F(<<"23.78">>), 23.78),
  ?assertEqual(G("foo", 111.2), 111.2).

to_atom_test() ->
  F = fun(X) -> coers:value(coers:to_atom(X)) end,
  ?assertEqual(F(foo), foo),
  ?assertEqual(F("foo"), foo),
  ?assertEqual(F(<<"foo">>), foo),
  ?assertEqual(F(222.987), '222.987').
