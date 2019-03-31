-module(web_common).
-export([
  send_200_ok_message/1,
  send_200_message/2,
  send_bad_request/1, send_bad_request/2,
  send_error_message/3, send_error_message/4,
  get_login_user_data/1]).

send_200_ok_message(Req) ->
  send_200_message(<<"ok">>, Req).

send_200_message(Message, Req) ->
  JsonMessage = jiffy:encode({[{message, Message}]}),
  cowboy_req:reply(200, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, JsonMessage, Req).

send_bad_request(Req) ->
  send_bad_request({content_type, application_json}, Req).
send_bad_request({content_type, ContentType}, Req) ->
  send_error_message(<<"Bad request">>, 400, ContentType, Req).

send_error_message(Message, StatusCode, Req) ->
  send_error_message(Message, StatusCode, application_json, Req).
send_error_message(Message, StatusCode, ContentType, Req) ->
  case ContentType of
    application_json ->
      JsonMessage = jiffy:encode({[{error, {[{message, Message}]}}]}),
      cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json; charset=utf-8">>}, JsonMessage, Req);
    _ -> %% text_plain
      cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Message, Req)
  end.

get_login_user_data({req, Req}) ->
  Cookies = cowboy_req:parse_cookies(Req),
  case lists:keyfind(<<"sessionid">>, 1, Cookies) of
    false ->
      {aborted, no_session};
    {_, SessionId} ->
      logger:notice("user session id : ~p", [SessionId]),
      case model_user_sup:user_info({session_id, SessionId}) of
        {aborted, Reason} ->
          {aborted, Reason};
        {ok, [{uuid, Uuid}, {email, Email}, {pw, Pw}, {name, Name}]} ->
          {ok, {{uuid, Uuid}, {email,Email}, {pw, Pw}, {name, Name}}}
      end
  end.