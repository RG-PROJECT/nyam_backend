%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2019 00:08
%%%-------------------------------------------------------------------
-module(nyam_backend_web_sup).
-author("chanwoonoh").

-behaviour(supervisor).

-include("../include/nyam_backend.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  WebWorker = ?CHILD(nyam_backend_web_worker, worker),
  {ok, {{one_for_one, 1, 5}, [WebWorker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
