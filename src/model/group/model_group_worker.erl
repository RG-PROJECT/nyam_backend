-module(model_group_worker).
-behaviour(gen_server).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	group_id, name, owner_uuid, members
}).

%% API.
-spec start_link({group_id, _}, {name, _}, {owner_uuid, _}, {members, _}) -> {ok, pid()}.
start_link({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {members, Members}) ->
	WorkerName = model_group_sup:get_worker_name(GroupId),
	logger:notice("group start_link ~p ~p ~p", [GroupId, GroupName, OwnerUuid]),
	gen_server:start_link({local, WorkerName}, ?MODULE, [{group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {members, Members}], []).

%% gen_server.

init(Args) ->
	{group_id, GroupId} = lists:keyfind(group_id, 1, Args),
	{name, GroupName} = lists:keyfind(name, 1, Args),
	{owner_uuid, OwnerUuid} = lists:keyfind(owner_uuid, 1, Args),
	{members, Members} = lists:keyfind(members, 1, Args),
	logger:notice("~p: group init ~p, ~p, ~p, ~p", [?MODULE, GroupId, GroupName, OwnerUuid, Members]),
	{ok, #state{group_id = GroupId, name = GroupName, owner_uuid = OwnerUuid, members = Members}}.

handle_call(ping, _From, State) ->
	{reply, pong, State};

handle_call({group_join, {user_uuid, Uuid}}, _From, State) ->
	case group_join_member({user_uuid, Uuid}, State) of
		{aborted, Reason} ->
			{reply, {aborted, Reason}, State};
		{ok, NewState} ->
			{reply, ok, NewState}
	end;

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

group_join_member({user_uuid, Uuid}, State) ->
	case is_member({uuid, Uuid}, State) of
		true -> {aborted, already_group_member};
		false ->
			logger:notice("NewMembers: ~p ++ ~p", [State#state.members, [Uuid]]),
			GroupId = State#state.group_id,
			NewMembers = State#state.members ++ [Uuid],
			NewState = State#state{members = NewMembers},
			case gen_server:call(db_worker, {group_member_update, {group_id, GroupId}, {new_members, NewMembers}}) of
				{aborted, Reason} ->
					{aborted, Reason};
				ok ->
					{ok, NewState}
			end
	end.

is_member({uuid, Uuid}, State) ->
	logger:notice("is_member ~p in ~p, ~p", [Uuid, State#state.owner_uuid, State#state.members]),
	(Uuid == State#state.owner_uuid) or (lists:member(Uuid, State#state.members)).