-module(model_user_sup).
-behaviour(supervisor).

-include("../../include/nyam_backend.hrl").
-include("./model_user_common.hrl").

-export([start_link/0]).
-export([init/1]).

-export([get_worker_name/1]).
-export([register/3, login/2, logout/1, user_info/1]).


-define(WORKER_SPEC(WORKER_ID, START_ARGS),
  {WORKER_ID, {model_user_worker, start_link, START_ARGS}, transient, 5000, worker, [model_user_worker]}).

-define(USER_SESSION_MAX_AGE_SEC, 604800).

-spec get_worker_name(list()) -> atom().
get_worker_name(Email) ->
  list_to_atom(?USER_WORKER_PREFIX ++ binary_to_list(Email)).

-spec get_worker_id(list()) -> binary().
get_worker_id(Email) ->
  list_to_binary(?USER_WORKER_PREFIX ++ binary_to_list(Email)).

%% TODO: ETS에서 sessionId 이용 데이터 꺼내올 때 expireTime 비교 후 오버될 경우 aborted & sessionId 제거
register({email, Email}, {pw, Password}, {name, Name}) ->
  logger:notice("UserSupervisor:register ~p ~p ~p", [Email, Password, Name]),
  case gen_server:call(db_worker, {user_insert, {email, Email}, {pw, Password}, {name, Name}}) of
    {already_exists, _} ->
      {aborted, user_already_exists};
    ok ->
      ok
  end.

login({email, Email}, {pw, Password}) ->
  logger:notice("UserSupervisor:login ~p ~p", [Email, Password]),
  %% user table 에서 사용자 정보 확인
  case gen_server:call(db_worker, {user_select, {email, Email}, {pw, Password}}) of
    {ok, undefined} ->
      {aborted, {no_exists, users}};
    {ok, UserInfoMap} ->
      %% 해당 정보로 프로세스 생성
      Name = maps:get(<<"name">>, UserInfoMap),
      Uuid = maps:get(<<"uuid">>, UserInfoMap),
      SessionId = make_session_id(Email),
      TargetWorkerId = get_worker_id(Email),
      WorkerName = get_worker_name(Email),
      {ok, WorkerName} = model_sup_manager:start_worker_if_not_exist({sup, model_user_sup}, {mod, model_user_worker},
        {id, TargetWorkerId}, {name, WorkerName}, {args, [{uuid, Uuid}, {email, Email}, {pw, Password}, {name, Name}]}),
      ets:insert(session_list, {SessionId, WorkerName, make_session_expire_time()}),
      logger:notice("User pid: ~p", [WorkerName]),
      gen_server:call(WorkerName, {echo, "Hi There"}),
      {ok, Name, SessionId, ?USER_SESSION_MAX_AGE_SEC}
  end.

logout({session_id, SessionId}) ->
  ets:delete(session_list, SessionId).

user_info({session_id, SessionId}) ->
  case ets:lookup(session_list, SessionId) of
    [] ->
      {aborted, not_found};
    [{_SessionId, ChildPid, ExpireTime}] ->
      case ExpireTime < current_gregorian_seconds() of
        true ->
          logout({session_id, SessionId}),
          {aborted, session_expired};
        _ ->
          {ok, gen_server:call(ChildPid, user_info)}
      end
  end.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  crypto:start(),
  ets:new(session_list, [public, named_table]),
  Procs = [],
  {ok, {{one_for_one, 1, 5}, Procs}}.


%% Private
make_session_id(Email) ->
  {A1, A2, A3} = erlang:timestamp(),
  Input = ?USER_WORKER_PREFIX ++ binary_to_list(Email) ++ integer_to_list(A1) ++ integer_to_list(A2) ++ integer_to_list(A3),
  <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Input),
  integer_to_binary(X, 16).
%%  integer_to_list(X, 16).

make_session_expire_time() ->
  current_gregorian_seconds() + ?USER_SESSION_MAX_AGE_SEC.

current_gregorian_seconds() ->
  calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(erlang:timestamp())).