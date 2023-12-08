-record(ptt_user_bio_table, {
	user_id=undefined, % Chosen by the user as a login ID (not the same as client_id)
	client_id=undefined, % client_key => binary UUID, i.e. <<"94be0c40-6d79-4611-87bc-8ee2d48b4dc4">>
	user_password=undefined,
	pending_password=undefined,
	login_token=undefined,
	log_in_time=undefined, % time stamp of last login
	log_in_state=undefined, 
	user_validated=false,
	permitions=0,
	user_image_file=undefined, % unique file name of the picture of this user.
	user_email=undefined, % the verified email of this user
	user_name=undefined, % the real name of this user. CANNOT BE SHOWN OR RETRIEVED BY ANYONE BUT AN ADMIN
	user_handle=undefined, % the public name this user is known by to the reset of the users.
	my_connections=[] % a list of ptt_user_bio_table entries. Each entry represents a person/group this use can chat with.
}).
