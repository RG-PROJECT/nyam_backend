-module(nyam_backend_app).
-behaviour(application).

-include("include/nyam_backend.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	application:start(sync),
	application:ensure_all_started(mongodb),
	application:ensure_all_started(cowboy),

	%% set debug for console logs
  %% lager:set_loglevel(lager_console_backend, debug),
	logger:set_primary_config(level,notice),
%%	Collection = <<"test">>,
%%	mc_worker_api:insert(Connection, Collection, #{<<"name">> => <<"Yankees">>}),
%%	mnesia:create_schema([node()]),
%%	init_mnesia(),
	init_mongodb(),
	StartRet = nyam_backend_sup:start_link(),
	model_group_sup:warm_up(),
	StartRet.


stop(_State) ->
	ok.


%%init_mnesia() ->
%%	mnesia:start(),
%%	logger:notice("Wait for DB Node response..."),
%%	case net_adm:ping(?DB_NODE) of
%%		pang ->
%%			timer:sleep(3000),
%%			init_mnesia();
%%		pong ->
%%			mnesia:change_config(extra_db_nodes, [?DB_NODE]),
%%			logger:notice(".....OK!")
%%	end.

init_mongodb() ->
	Database = <<"test">>,
	{ok, _Connection} = mc_worker_api:connect ([{database, Database}]).