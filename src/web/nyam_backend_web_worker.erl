%%%-------------------------------------------------------------------
%%% @author chanwoonoh
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2019 00:09
%%%-------------------------------------------------------------------
-module(nyam_backend_web_worker).
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

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/", web_index_handler, []}]
    ++ gen_route_users()
    ++ gen_route_groups()}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{
      env => #{dispatch => Dispatch},
      middlewares => [nyam_backend_web_middleware, cowboy_router, cowboy_handler]
    }
  ),
  logger:notice("http://127.0.0.1:8080/"),
  {ok, #state{}}.

gen_route_users() ->
  Prefix = "/users",
  Routes0 = [
    {"/login", web_users_login_handler, []},
    {"/logout", web_users_logout_handler, []},
    {"/me", web_users_me_handler, []},
    {"/register", web_users_register_handler, []}
  ],
  [{Prefix ++ Path, Handler, Opts} || {Path, Handler, Opts} <- Routes0].

gen_route_groups() ->
  Prefix = "/groups",
  Routes0 = [
    {"/:groupId/create", web_groups_create_handler, []},
    {"/:groupId/join", web_groups_join_handler, []},
    {"/:groupId/leave", web_groups_leave_handler, []}
  ],
  [{Prefix ++ Path, Handler, Opts} || {Path, Handler, Opts} <- Routes0].

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
