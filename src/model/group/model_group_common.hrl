%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, rajephon
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2019 15:36
%%%-------------------------------------------------------------------
-author("chanwoonoh").

-include_lib("stdlib/include/qlc.hrl").

-define(GROUP_WORKER_PREFIX, "WORKER_GR0UP__").

-record(groups, {
  uuid, group_id, name, owner, members
}).
