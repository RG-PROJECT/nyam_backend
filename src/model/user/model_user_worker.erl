-module(model_user_worker).
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
	uuid, email, pw, name
}).

%%-define(SERVER,?MODULE).

%% API.
%%-spec start_link({email, Email}, {pw, Password}) -> {ok, pid()}.
start_link({uuid, Uuid}, {email, Email}, {pw, Password}, {name, Name}) ->
	WorkerName = model_user_sup:get_worker_name(Email), %% list_to_atom(?USER_WORKER_PREFIX ++ binary_to_list(Email)),
	logger:notice("user start_link ~p ~p, ~p", [WorkerName, Email, Password]),
	gen_server:start_link({local, WorkerName}, ?MODULE, [{uuid, Uuid}, {email,Email}, {pw,Password}, {name, Name}], []).

%% gen_server.

init(Args) ->
	Uuid = lists:keyfind(uuid, 1, Args),
	Email = lists:keyfind(email, 1, Args),
	Password = lists:keyfind(pw, 1, Args),
	Name = lists:keyfind(name, 1, Args),
	logger:notice("~p: user init ~p, ~p, ~p ~p", [?MODULE, Uuid, Email, Password, Name]),
	{ok, #state{uuid=Uuid, email=Email, pw=Password, name=Name}}.

handle_call(ping, _From, State) ->
	{reply, pong, State};

handle_call({echo,Message}, _From, State) ->
	logger:notice("[user_worker:~p]Received Echo ~s~n", [State#state.email, Message]),
	{reply, ignored, State};

handle_call(user_info, _From, State) ->
	logger:notice("[user_worker:~p]Received UserInfo ~s~n", [State#state.email]),
	{reply, [State#state.uuid, State#state.email, State#state.pw, State#state.name], State};

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