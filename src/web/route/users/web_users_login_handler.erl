-module(web_users_login_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Req = request_handle(Method, HasBody, Req0),
  {ok, Req, Opts}.

request_handle(<<"POST">>, true, Req0) ->
  {ok, Params, Req} = cowboy_req:read_urlencoded_body(Req0),
  Email = proplists:get_value(<<"email">>, Params),
  Pw = proplists:get_value(<<"pw">>, Params),
  login_process(Email, Pw, Req);
request_handle(<<"POST">>, false, Req) ->
  web_common:send_error_message(<<"Missing body">>, 400, Req);
request_handle(_, _, Req) ->
  web_common:send_error_message(<<"Method not allowed">>, 405, Req).

login_process(Email, Password, Req0) when is_bitstring(Email), is_bitstring(Password) ->
  case model_user_sup:login({email, Email}, {pw, Password}) of
    {ok, Name, SessionId, SessionMaxAge} ->
      logger:notice("Login success. Welcome ~p ~p", [Name, SessionId]),
      Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionId, Req0, #{max_age => SessionMaxAge, path => "/"}),
      web_common:send_200_ok_message(Req);
    {aborted, {no_exists, users}} ->
      logger:error("Login failed."),
      web_common:send_error_message(<<"Login Failed">>, 403, Req0);
    {aborted, Reason} -> %% TODO: send response 500 error.
      logger:error("Login Aborted ~p", [Reason]),
      web_common:send_error_message(<<"500 Internal Error">>, 500, Req0)
  end;
login_process(_, _, Req) ->
  web_common:send_bad_request(Req).

