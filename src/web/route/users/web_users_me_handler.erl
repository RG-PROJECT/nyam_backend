-module(web_users_me_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, Opts) ->
%%	model_user_sup:login({email, "EEEMail"}, {pw, "Password"}),
%%	Req = web_common:send_200_message(<<"Hello Erlang! Me">>, Req0),
%%	{ok, Req, State}.
  Method = cowboy_req:method(Req0),
  Req = request_handle(Method, Req0),
  {ok, Req, Opts}.

request_handle(<<"GET">>, Req0) ->
  case web_common:get_login_user_data({req, Req0}) of
    {aborted, no_session} ->
      web_common:send_error_message(<<"Need Login">>, 402, Req0);
    {aborted, not_found} ->
      web_common:send_error_message(<<"User data not found.">>, 404, Req0);
    {aborted, session_expired} ->
      web_common:send_error_message(<<"Login session expired.">>, 402, Req0);
    {ok, {{uuid, _}, {email, Email}, {pw, _}, {name, Name}}} ->
      logger:notice("Email: ~p, Name: ~p", [Email, Name]),
      JsonMessage = jiffy:encode({[{email, Email}, {name, Name}]}),
      cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, JsonMessage, Req0)
  end;

request_handle(_, Req) ->
  web_common:send_error_message(<<"Method not allowed">>, 405, Req).