%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2019 16:13
%%%-------------------------------------------------------------------
-module(user_spec).
-author("chanwoonoh").

-include_lib("eunit/include/eunit.hrl").

-include("../src/model/user/model_user_common.hrl").


simple_test() ->
  logger:notice("Hi there."),
  application:start(mnesia),
  F = fun() -> mnesia:select(user, [{'_', [], ['$_']}]) end,
  Result = mnesia:transaction(F),
  logger:notice("Result: ~p", [Result]),
  ?assert(true).

