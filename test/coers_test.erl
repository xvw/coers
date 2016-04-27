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
