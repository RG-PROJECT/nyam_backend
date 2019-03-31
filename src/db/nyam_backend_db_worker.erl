%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2019 14:35
%%%-------------------------------------------------------------------
-module(nyam_backend_db_worker).
-author("chanwoonoh").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, db_worker).

-record(state, {connection}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Connection} = db_connect(),
  logger:notice("Database connection is successful"),
  {ok, #state{connection = Connection}}.

handle_call(ping, _From, State) ->
  {reply, pong, State};
handle_call({user_insert, {email, Email}, {pw, Password}, {name, Name}}, _From, State) ->
  Conn = State#state.connection,
  case user_insert({email, Email}, {pw, Password}, {name, Name}, {conn, Conn}) of
    {{true, _}, _} ->
      {reply, ok, State};
    {already_exists,  UserInfo} ->
      {reply, {already_exists,  UserInfo}, State}
  end;
handle_call({user_select, {email, Email}}, _From, State) ->
  Conn = State#state.connection,
  UserInfo = user_select({email, Email}, {conn, Conn}),
  {reply, {ok, UserInfo}, State};
handle_call({user_select, {email, Email}, {pw, Password}}, _From, State) ->
  Conn = State#state.connection,
  UserInfo = user_select({email, Email}, {pw, Password}, {conn, Conn}),
  {reply, {ok, UserInfo}, State};

handle_call(group_all, _From, State) ->
  Conn = State#state.connection,
  AllGroups = group_all({conn, Conn}),
  {reply, AllGroups, State};

handle_call({group_insert, {group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}}, _From, State) ->
  Conn = State#state.connection,
  case group_insert({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {conn, Conn}) of
    {{true, _}, _} ->
      {reply, ok, State};
    {already_exists,  GroupInfo} ->
      {reply, {already_exists,  GroupInfo}, State}
  end;
handle_call({group_member_update, {group_id, GroupId}, {new_members, NewMembers}}, _From, State) ->
  Conn = State#state.connection,
  {reply, group_member_update({group_id, GroupId}, {new_members, NewMembers}, {conn, Conn}), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% -----------------------------------------------------------

db_connect() ->
  Database = <<"test">>,
  case catch mc_worker_api:connect ([{database, Database}]) of
    {error, Reason} ->
      logger:error("DB connection error: ~p~n", [Reason]),
      error;
    {ok, Connection} ->
      {ok, Connection}
  end.

user_select({email, Email}, {conn, Conn}) when is_list(Email) ->
  user_select({email, list_to_binary(Email)}, {conn, Conn});

user_select({email, Email}, {conn, Conn}) when is_binary(Email) ->
  mc_worker_api:find_one(Conn, <<"Users">>, #{<<"email">> => Email}).

user_select({email, Email}, {pw, Password}, {conn, Conn}) when is_list(Email), is_list(Password) ->
  user_select({email, list_to_binary(Email)}, {pw, list_to_binary(Password)}, {conn, Conn});

user_select({email, Email}, {pw, Password}, {conn, Conn}) ->
  mc_worker_api:find_one(Conn, <<"Users">>, #{<<"email">> => Email, <<"pw">> => Password}).

user_insert({email, Email}, {pw, Password}, {name, Name}, {conn, Conn}) ->
  case user_select({email, Email}, {conn, Conn}) of
    undefined ->
      mc_worker_api:insert(Conn, <<"Users">>,
        #{<<"uuid">> => uuid:uuid_to_string(uuid:get_v4()), <<"email">> => Email, <<"pw">> => Password, <<"name">> => Name});
    UserInfo ->
      {already_exists,  UserInfo}
  end.

group_all({conn, Conn}) ->
  logger:notice("call group_all"),
  case mc_worker_api:find(Conn, <<"Groups">>, #{}) of
    [] -> [];
    {ok, Cursor} ->
      Result = get_all_cursor_data(Cursor),
      mc_cursor:close(Cursor),
      Result
  end.

group_insert({group_id, GroupId}, {name, GroupName}, {owner_uuid, OwnerUuid}, {conn, Conn}) ->
  case group_select({group_id, GroupId}, {conn, Conn}) of
    undefined ->
      mc_worker_api:insert(Conn, <<"Groups">>,
        #{<<"uuid">> => uuid:uuid_to_string(uuid:get_v4()), <<"group_id">> => GroupId, <<"group_name">> => GroupName, <<"owner">> => OwnerUuid, <<"members">> => []});
    GroupInfo ->
      {already_exists, GroupInfo}
  end.

group_member_update({group_id, GroupId}, {new_members, NewMembers}, {conn, Conn}) ->
  Command = #{<<"$set">> => #{<<"members">> => NewMembers}},
  case mc_worker_api:update(Conn, <<"Groups">>, #{<<"group_id">> => GroupId}, Command) of
    {false, Reason} ->
      logger:notice("group_member_update false : ~p", [Reason]),
      {aborted, Reason};
    {true, Result} ->
      logger:notice("group_member_update true : ~p", [Result]),
      ok
  end.

group_select({group_id, GroupId}, {conn, Conn}) ->
  mc_worker_api:find_one(Conn, <<"Groups">>, #{<<"group_id">> => GroupId}).

get_all_cursor_data(Cursor) ->
  case mc_cursor:next(Cursor) of
    {T} -> [T] ++ get_all_cursor_data(Cursor);
    _ -> []
  end.