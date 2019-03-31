-module(nyam_backend_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("include/nyam_backend.hrl").

%%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% lists:keyfind(nyam_backend_web_sup, 1, Childs).

init([]) ->
	Web = ?CHILD(nyam_backend_web_sup, supervisor),
	User = ?CHILD(model_user_sup, supervisor),
	Group = ?CHILD(model_group_sup, supervisor),
	Db = ?CHILD(nyam_backend_db_sup, supervisor),
%%  Db = ?CHILD(nyam_backend_db_sup, supervisor),
	{ok, {{one_for_one, 1, 5}, [Web, User, Group, Db]}}.
