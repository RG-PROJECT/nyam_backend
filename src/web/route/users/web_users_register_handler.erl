-module(web_users_register_handler).
-behavior(cowboy_handler).

-export([init/2]).

-define(REGEX_EMAIL_PATTERN, "\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*\\b").
-define(REGEX_PASSWORD_PATTERN, "^(?=.*?[a-zA-Z])(?=.*?[0-9])(?=.*?[#?!@$%^&*-]).{6,32}$").
-define(REGEX_NAME_PATTERN, "^[^\s]{2,12}(\s+[^\s]+)*$").

%% post method. email, pw, name
%% TODO: sessionid 쿠키 가지고 있으면 403처리
init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Req = request_handle(Method, HasBody, Req0),
  {ok, Req, State}.

request_handle(<<"POST">>, true, Req0) ->
  {ok, Params, Req} = cowboy_req:read_urlencoded_body(Req0),
  Email = proplists:get_value(<<"email">>, Params),
  Pw = proplists:get_value(<<"pw">>, Params),
  Name = proplists:get_value(<<"name">>, Params),
  case input_validation(Email, Pw, Name) of
    false -> web_common:send_error_message(<<"Invalid input">>, 403, Req);
    true -> register_process(Email, Pw, Name, Req)
  end;
request_handle(<<"POST">>, false, Req) ->
  web_common:send_error_message(<<"Missing body">>, 400, Req);
request_handle(_, _, Req) ->
  web_common:send_error_message(<<"Method not allowed">>, 405, Req).


input_validation(Email, Pw, Name) ->
  re:run(binary_to_list(Email), ?REGEX_EMAIL_PATTERN) /= nomatch andalso
  re:run(binary_to_list(Pw), ?REGEX_PASSWORD_PATTERN) /= nomatch andalso
  re:run(binary_to_list(Name), ?REGEX_NAME_PATTERN) /= nomatch.

register_process(Email, Pw, Name, Req) ->
  case model_user_sup:register({email, Email}, {pw, Pw}, {name, Name}) of
    {aborted, user_already_exists} ->
      web_common:send_error_message(<<"Email already exists in db">>, 409, Req);
    {aborted, Reason} ->
      logger:error("register_process error ~n~p ", [Reason]),
      web_common:send_error_message(<<"Server internal error">>, 500, Req);
    ok ->
      web_common:send_200_ok_message(Req)
  end.

