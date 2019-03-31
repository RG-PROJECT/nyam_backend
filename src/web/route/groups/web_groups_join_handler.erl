-module(web_groups_join_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0=#{method := <<"POST">>}, State) ->
	GroupId = cowboy_req:binding(groupId, Req0),
	Req = request_handle({group_id, GroupId}, Req0),
	{ok, Req, State};

init(Req0, State) ->
	Req = web_common:send_error_message(<<"Method not allowed">>, 405, Req0),
	{ok, Req, State}.

request_handle({group_id, GroupId}, Req) ->
	logger:notice("RequestHandle ~p", [GroupId]),
	case web_common:get_login_user_data({req, Req}) of
		{aborted, no_session} ->
			web_common:send_error_message(<<"need login">>, 402, Req);
		{aborted, Reason} ->
			logger:error("group create error: ~p", [Reason]),
			web_common:send_error_message(<<"Server Internal Error">>, 500, Req);
		{ok, {{uuid, Uuid}, {email, _Email}, {pw, _Pw}, {name, _Name}}} ->
			GroupJoinResult = model_group_sup:join_group({group_id, GroupId}, {user_uuid, Uuid}),
			logger:notice("RES: ~p", [GroupJoinResult]),
			case GroupJoinResult of
				{aborted, already_group_member} ->
					web_common:send_error_message(<<"already group member">>, 409, Req);
				{aborted, Reason} ->
					logger:error("join group aborted: ~p", [Reason]),
					web_common:send_error_message(<<"Server internal error">>, 500, Req);
				ok ->
					web_common:send_200_ok_message(Req)
			end
	end.

