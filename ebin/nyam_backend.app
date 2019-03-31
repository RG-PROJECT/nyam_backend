{application, 'nyam_backend', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['model_group_sup','model_group_worker','model_sup_manager','model_user_sup','model_user_worker','nyam_backend_app','nyam_backend_db_sup','nyam_backend_db_worker','nyam_backend_sup','nyam_backend_web_middleware','nyam_backend_web_sup','nyam_backend_web_worker','uuid','web_common','web_groups_create_handler','web_groups_join_handler','web_groups_leave_handler','web_index_handler','web_users_login_handler','web_users_logout_handler','web_users_me_handler','web_users_register_handler']},
	{registered, [nyam_backend_sup]},
	{applications, [kernel,stdlib,sync,cowboy,mongodb,jiffy,mongodb]},
	{mod, {nyam_backend_app, []}},
	{env, []},
	{included_applications, ['mnesia']}
]}.