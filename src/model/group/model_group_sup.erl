-module(model_group_sup).
-behaviour(supervisor).

-include("./model_group_common.hrl").

-export([start_link/0]).
-export([init/1]).

%%-export([create/2]).
-export([
	warm_up/0,
	create_group/3,
	join_group/2,
	get_worker_name/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

-spec get_worker_name(list()) -> atom().
get_worker_name(GroupId) ->
	list_to_atom(?GROUP_WORKER_PREFIX ++ binary_to_list(GroupId)).

-spec get_worker_id(list()) -> binary().
get_worker_id(GroupId) ->
	list_to_binary(?GROUP_WORKER_PREFIX ++ binary_to_list(GroupId)).

%% TODO: supervisor가 가동되면, DB에서 모든 group 정보를 가져와서, 프로세스로 띄운다.
warm_up() ->
	AllGroups = gen_server:call(db_worker, group_all),
	TotalGroups = length(AllGroups),
	logger:notice("warm_up: Load All Group workers ~p", [TotalGroups]),
	lists:foreach(
		fun(Group) ->
			GroupId = maps:get(<<"group_id">>, Group),
			GroupName = maps:get(<<"group_name">>, Group),
			OwnerUuid = maps:get(<<"owner">>, Group),
			Members = maps:get(<<"members">>, Group),
			start_group_worker({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {members, Members})
		end,
		AllGroups),
	ok.

create_group({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}) ->
	logger:notice("GroupSupervisor:create: ~p ~p ~p", [GroupId, GroupName, OwnerUuid]),
	case gen_server:call(db_worker, {group_insert, {group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}}) of
		{already_exists, _} ->
			{aborted, group_already_exists};
		ok ->
			start_group_worker({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {members, []})
	end.

join_group({group_id, GroupId}, {user_uuid, Uuid}) ->
	logger:notice("GroupSupervisor:join: ~p ~p", [GroupId, Uuid]),
	WorkerName = get_worker_name(GroupId),
	gen_server:call(WorkerName, {group_join, {user_uuid, Uuid}}).
%%	case gen_server:call()
%%	case model_group_db:insert_group({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}) of
%%		{atomic, {already_exist, [{Uuid, _Id, _Name}]}} ->
%%			logger:notice("already exist: ~p ~p ~p", [Uuid, _Id, _Name]),
%%			{already_exist, {Uuid, _Id, _Name}};
%%		{ok, UUid} ->
%%			logger:notice("disk write success. ~p", [UUid]);
%%		R ->
%%			logger:notice("Whatth ~p", [R])
%%%%		 TODO: create worker process
%%	end.


start_group_worker({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {members, Members}) ->
	TargetWorkerId = get_worker_id(GroupId),
	WorkerName = get_worker_name(GroupId),
	{ok, WorkerName} = model_sup_manager:start_worker_if_not_exist({sup, model_group_sup}, {mod, model_group_worker},
		{id, TargetWorkerId}, {name, WorkerName}, {args, [{group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {members, Members}]}),
	logger:notice("Start Group Worker : ~p", [WorkerName]),
	gen_server:call(WorkerName, {echo, "Hi There"}),
	ok.

%%	ok = model_user_db:init(),
%%	R = model_user_db:select_user({email, "rajephon@gmail.com"}, {pw, "PASS@WORD"}),
%%	logger:notice("Select Users: ~p", [R]),
%%%%  {atomic, [{users, Uuid, Email, Pw, UserName}]} = model_user_db:select_user({email, "rajephon@gmail.com"}, {pw, "PASS@WORD"}),
%%%%  logger:notice("Select Users: ~p ~p ~p ~p", [uuid:uuid_to_string(Uuid), Email, Pw, UserName]),
%%	%% create ets table session_list
%%	ets:new(session_list, [public, named_table]),

