-record(users_login_table, {
	user_id=undefined, % Chosen by the user as a login ID (not the same as client_id)
	client_id=undefined, % client_key => binary UUID, i.e. <<"94be0c40-6d79-4611-87bc-8ee2d48b4dc4">>
	user_password=undefined,
	login_token=undefined,
	log_in_time=undefined, % time stamp of last login
	log_in_state=undefined, 
	user_validated=false,
	permitions=0
}).
