%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, rajephon
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2019 13:32
%%%-------------------------------------------------------------------
-author("chanwoonoh").

-include_lib("stdlib/include/qlc.hrl").

-define(USER_WORKER_PREFIX, "WORKER_US3R__").

-record(users, {
  uuid, email, pw, name
}).

