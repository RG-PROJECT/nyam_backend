-module(web_groups_create_handler).
-behavior(cowboy_handler).

-export([init/2]).

-define(REGEX_GROUP_NAME_PATTERN, "^[^\s]{3,10}$").

init(Req1=#{method := <<"POST">>}, State) ->
	{ok, Params, Req0} = cowboy_req:read_urlencoded_body(Req1),
	Req = case proplists:get_value(<<"groupname">>, Params) of
		undefined ->
			web_common:send_bad_request(Req0);
		GroupName ->
			case input_validation(GroupName) of
				true ->
					GroupId = cowboy_req:binding(groupId, Req0),
					request_handle({group_id, GroupId}, {group_name, GroupName}, Req1);
				false ->
					web_common:send_bad_request(Req0)
			end
	end,
	{ok, Req, State};

init(Req0, State) ->
	Req = web_common:send_error_message(<<"Method not allowed">>, 405, Req0),
	{ok, Req, State}.

input_validation(GroupName) ->
	re:run(binary_to_list(GroupName), ?REGEX_GROUP_NAME_PATTERN) /= nomatch.

request_handle({group_id, GroupId}, {group_name, GroupName}, Req) ->
	logger:notice("RequestHandle ~p ~p", [GroupId, GroupName]),
	case web_common:get_login_user_data({req, Req}) of
		{aborted, no_session} ->
			web_common:send_error_message(<<"need login">>, 402, Req);
		{aborted, Reason} ->
			logger:error("group create error: ~p", [Reason]),
			web_common:send_error_message(<<"Server Internal Error">>, 500, Req);
		{ok, {{uuid, Uuid}, {email, _Email}, {pw, _Pw}, {name, _Name}}} ->
			GroupCreateResult = model_group_sup:create_group({group_id, GroupId}, {name, GroupName}, {owner_uuid, Uuid}),
			logger:notice("RES: ~p", [GroupCreateResult]),
			case GroupCreateResult of
				{aborted, group_already_exists} ->
					web_common:send_error_message(<<"an existing group id">>, 409, Req);
				{aborted, Reason} ->
					logger:error("create group aborted: ~p", [Reason]),
					web_common:send_error_message(<<"Server internal error">>, 500, Req);
				ok ->
					web_common:send_200_ok_message(Req)
			end
	end.