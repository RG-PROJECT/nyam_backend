%%%-------------------------------------------------------------------
%%% @author Chanwoo noh
%%% @copyright (C) 2019, Chanwoo Noh <rajephon@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2019 14:21
%%%-------------------------------------------------------------------
-module(nyam_backend_db_sup).
-author("Chanwoo Noh <rajephon@gmail.com>").

-behaviour(supervisor).

-include("../include/nyam_backend.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Worker = ?CHILD(nyam_backend_db_worker, worker),
  {ok, {{one_for_one, 1, 5}, [Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
