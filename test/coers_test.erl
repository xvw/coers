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
