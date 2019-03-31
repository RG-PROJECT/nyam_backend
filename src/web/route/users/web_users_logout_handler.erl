-module(web_users_logout_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  Req = request_handle(Method, Req0),
  {ok, Req, Opts}.

request_handle(<<"POST">>, Req0) ->
  Cookies = cowboy_req:parse_cookies(Req0),
  case lists:keyfind(<<"sessionid">>, 1, Cookies) of
    {_, SessionId} ->
      logger:notice("Logout: ~p", [SessionId]),
      model_user_sup:logout({session_id, SessionId})
  end,
  web_common:send_200_ok_message(Req0);
request_handle(_, Req) ->
  web_common:send_error_message(<<"Method not allowed">>, 405, Req).