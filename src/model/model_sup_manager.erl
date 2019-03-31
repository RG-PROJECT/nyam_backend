%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2019 17:15
%%%-------------------------------------------------------------------
-module(model_sup_manager).
-author("chanwoonoh").

%% API
-export([start_worker_if_not_exist/5]).

-define(WORKER_SPEC(WORKER_ID, WORKER_MOD, START_ARGS),
  {WORKER_ID, {WORKER_MOD, start_link, START_ARGS}, transient, 5000, worker, [WORKER_MOD]}).

-spec start_worker_if_not_exist({sup, atom()}, {mod, atom()}, {id, atom()}, {name, binary()}, {args, list()}) -> tuple().
start_worker_if_not_exist({sup, Sup}, {mod, Mod}, {id, Id}, {name, Name}, {args, Args}) ->
  case catch gen_server:call(Name, ping) of
    pong ->
      logger:notice("already exists. ~p~n", [Name]),
      {ok, Name};
    _ ->
      logger:notice("~p start_child ~p ~p ~p ", [Sup, Name, Id, Args]),
      case supervisor:start_child(Sup,
        ?WORKER_SPEC(Id, Mod, Args)) of
        {error, Reason} ->
          {error, Reason};
        {ok, _Child} ->
          {ok, Name};
        {ok, _Child, _} ->
          {ok, Name}
      end
  end.