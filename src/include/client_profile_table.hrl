-record(client_profile_table, {
	client_id, % client_key => binary UUID, i.e. <<"94be0c40-6d79-4611-87bc-8ee2d48b4dc4">>
	youtube_key,
	channel_list % channel_list => a list of tuples, i.e. [{<<"pbs space time">>, <<"UC7_gcs09iThXybpVgjHZ_7g">>}, ..., {name, channelID}]
}).

-record(client_profile_table_pending, {
	client_id, %%% binary UUID, i.e. <<"94be0c40-6d79-4611-87bc-8ee2d48b4dc4">>
	video_id_list %%% a list of channel IDs
}).