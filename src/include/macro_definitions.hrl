-ifdef(debug).
-define(BUILDTYPE, <<"debug">>).
-else.
-define(BUILDTYPE, <<"RELEASE">>).
-endif.


-define(HTTP_PORT, 8080).
-define(HTTP_PORT_LOCAL, 8083).
-define(HTTP_ACTIVE_PORT, ?HTTP_PORT_LOCAL).
-define(LOGIN_CALLBACK_ADDRESS_LOCAL, <<"http://10.0.0.16:8083/">>).
-define(LOGIN_CALLBACK_ADDRESS_REMOTE, <<"http://35.208.173.235:8080/">>).
-define(LOGIN_CALLBACK_ADDRESS_ACTIVE, ?LOGIN_CALLBACK_ADDRESS_LOCAL).

-define(LOGIN_STATE_USER_NOT_FOUND, 16#AAAA00).
-define(LOGIN_STATE_EMAIL_SENT, 16#AAAA01).
-define(LOGIN_STATE_TOKEN_EXPIRED, 16#AAAA02).
-define(LOGIN_STATE_CONFIRMED, 16#AAAA03).
-define(LOGIN_STATE_LOGGEDOUT, 16#AAAA04).
-define(LOGIN_STATE_RESET_PASSWORD, 16#AAAA05).
-define(LOGIN_STATE_RESET_USER, 16#AAAA06).
-define(LOGIN_STATE_NOT_LOGGED_IN, 16#AAAA07).
-define(LOGIN_STATE_LOGGED_IN, 16#AAAA08).
-define(LOGIN_ERROR_PASSWORD, 16#AAAA09).
-define(LOGIN_STATE_LOGIN_EXPIRED, 16#AAAA0A).
-define(LOGIN_STATE_EMAIL_NOTIFICATION_FAILED, 16#AAAA0B).
-define(LOGIN_STATE_IDENTICAL_PASSWORDS, 16#AAAA0C).
-define(LOGIN_TOKEN_EXPIRATION_TIME, 60*60*1000).

-define(NASA_IMAGES_HOST, "https://images-api.nasa.gov/search?").
-define(APOD_HOST, "https://api.nasa.gov/planetary/apod?").
-define(YOUTUBE_HOST, "https://www.googleapis.com/youtube/v3/search?").
-define(ASTRONOMY_API_KEY, "K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8").
-define(YOUTUBE_CHANNEL_IDS, [<<"UC7_gcs09iThXybpVgjHZ_7g">>, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>]).
-define(CLIENT_ACCESS_KEY, <<"389b4e36-b3d2-11ed-afa1-0242ac120002">>).
-define(ADMINISTRATOR_KEY, <<"f09a2270-ac19-418c-a443-9f9e4f4c9019">>).
-define(YOUTUBE_MAXRESULTS, "50").
-define(FIRST_PUBLISH_DATE, "1970-01-01T00:00:00Z").
-define(SERVER_CONFIG_FILE, <<"server_config.cfg">>).
-define(LOGIN_EMAIL_SENDER, "TheTinkerersShop@gmail.com").
-define(MAILJET_ENDPOINT, "https://api.mailjet.com/v3.1/send").
-define(YOUTUBE_NEXTPAGE, <<"nextPageToken">>).
-define(TOP_KEY, <<"Videos">>).
-define(YOUTUBE_VIDEO_ARRAY_KEY, <<"items">>).
-define(YOUTUBE_VIDEO_ID_KEY, <<"videoId">>).
-define(YOUTUBE_RETURN_VIEDO_LIST_KEY, <<"Videos">>).
-define(ADMIN_AVAILABLE_ACTIONS, [<<"delete">>, <<add>>]).
-define(YOUTUBE_KEY, <<"youtubekey">>).
-define(REQUIRED_TOKEN_USERID, <<"user_id">>).
-define(REQUIRED_TOKEN_USERPASSWORD, <<"user_password">>).
-define(REQUIRED_CLIENT_KEY_TOKEN, <<"key">>).
-define(REQUIRED_ACTION_TOKEN, <<"action">>).
-define(REQUIRED_CHANNEL_ID_TOKEN, <<"channel_id">>).
-define(REQUIRED_CLIENT_ID_TOKEN, <<"client_id">>).
-define(LOGIN_EMAIL_FROM_NAME, <<"Sonoma Ashram">>).
-define(LOGIN_EMAIL_SUBJECT, <<"Please complete your login">>).
-define(LOGIN_EMAIL_LINK, <<"<a href=\"click me\">www.google.com</a>">>).
-define(AVAILABLE_CHANNEL_ACTIONS, [<<"fetchclientdirectory">>,
									<<"fetchchannelvideos">>,
									<<"updatechannel">>,
									<<"addvideolink">>, 
									<<"linkstatus">>, 
									<<"fetchclientprofile">>, 
									<<"updateclientprofile">>, 
									<<"fetchchannelimage">>,
									<<"deletechannel">>,
									<<"restoredefaultclient">>,
									<<"login_query">>,
									<<"login_new_userid">>,
									<<"login_new_password">>,
									<<"login_reset_password">>,
									<<"login_validate_token">>,
									<<"login_existing">>,
									<<"clear_login_table">>,
									<<"complete_login">>,
									<<"user_profile">>]).
-define(EXCEPTIONAL_ACTIONS, [<<"complete_login">>, <<"complete_password_reset">>]).
-define(PUT_ACTIONS, [<<"logout_user">>]).

-define(LOGIN_ID_TOKEN, "id:").
-define(LOGIN_PASSWORD_TOKEN, "password:").

-define(SERVER_ERROR_OK, 					16#FFAA00).
-define(SERVER_ERROR_BAD_CLIENT_ID, 		16#FFAA01).
-define(SERVER_ERROR_MISSING_ACTION, 		16#FFAA02).
-define(SERVER_ERROR_MISSING_PARAMETER, 	16#FFAA03).
-define(SERVER_ERROR_MALFORMED_COMMAND, 	16#FFAA04).
-define(SERVER_ERROR_INVALID_COMMAND, 		16#FFAA05).
-define(SERVER_ERROR_DB_ERROR, 				16#FFAA06).
-define(SERVER_ERROR_NOT_FOUND, 			16#FFAA07).
-define(SERVER_ERROR_AUTHENTICATION, 		16#FFAA08).
-define(SERVER_ERROR_INVALID_ACTION,		16#FFAA09).
-define(SERVER_ERROR_MISSING_CHANNEL,		16#FFAA0A).
-define(SERVER_ERROR_MISSING_CLIENT,		16#FFAA0B).
-define(SERVER_ERROR_INVALID_CLIENT,		16#FFAA0C).
-define(SERVER_ERROR_MISSING_VIDEO,			16#FFAA0D).
-define(SERVER_ERROR_INVALID_LINK,			16#FFAB00).
-define(SERVER_ERROR_LINK_EXISTS, 			16#FFAB01).
-define(SERVER_ERROR_LINK_PENDING, 			16#FFAB02).
-define(SERVER_ERROR_LINK_ADDED,			16#FFAB03).
-define(SERVER_ERROR_LINK_DENIED, 			16#FFAB04).
-define(SERVER_ERROR_LINK_NOT_FOUND,		16#FFAB05).
-define(SERVER_ERROR_RECORD_NOT_FOUND,		16#FFAA11).
-define(SERVER_ERROR_ADMIN_KEY,				16#FFAA12).
-define(SERVER_ERROR_NO_ACTION_SPECIFIED,	16#FFAA13).
-define(SERVER_ERROR_ACTION_TYPE,			16#FFAA14).
-define(SERVER_ERROR_NO_SUCH_CLIENT,		16#FFAA15).
-define(SERVER_ERROR_INVALID_PARAMETER,		16#FFAA16).
-define(SERVER_ERROR_NOT_IMPLEMENTED,		16#FFAA17).
-define(SERVER_ERROR_NO_SUCH_CHANNEL,		16#FFAA18).
-define(SERVER_ERROR_NO_SUCH_COMMAND,		16#FFAA19).
-define(SERVER_ERROR_DUPLICATE_CHANNEL,		16#FFAA1A).
-define(SERVER_ERROR_NO_RECORDS_FOUND,		16#FFAA1B).
-define(SERVER_ERROR_TARGET_ID_REQUIRED, 	16#FFAC00).
-define(SERVER_ERROR_CHANNEL_DATA_REQUIRED,	16#FFAC01).
-define(SERVER_ERROR_INVALID_CHANNEL_DATA, 	16#FFAC02).
-define(SERVER_ERROR_VIDEO_LINK_REQUIRED, 	16#FFAC03).
-define(SERVER_ERROR_NO_SUCH_ENDPOINT, 		16#FFAC04).
-define(SERVER_ERROR_USER_LOGIN,	 		16#FFAC05).
-define(SERVER_ERROR_USER_ID,		 		16#FFAC06).
-define(SERVER_ERROR_USER_PASSWORD,	 		16#FFAC07).
-define(SERVER_ERROR_NO_LOGIN_RECORD, 		16#FFAC08).
-define(SERVER_ERROR_USER_EXISTS, 			16#FFAC09).
-define(SERVER_ERROR_LOGIN_INVALID_STATE,	16#FFAC0A).
-define(SERVER_ERROR_NO_SUCH_USER_ID, 		16#FFAC0B).
-define(SERVER_ERROR_MISSING_TOKEN, 		16#FFAC0C).
-define(SERVER_ERROR_INVALID_EMAIL, 		16#FFAC0D).
-define(SERVER_ERROR_INVALID_CREDENTIALS,	16#FFAC0E).
%%%%%%%%%%%%% System faults &&&&&&&&&&&&&&&&&&&&&&
-define(SERVER_ERROR_DATABASE_FAULT,		16#FFFF01).

-define(RESPONSE_CODES, [{video_link_added, ?SERVER_ERROR_LINK_ADDED},
						 {video_link_exists, ?SERVER_ERROR_LINK_EXISTS}, 
						 {link_pending, ?SERVER_ERROR_LINK_PENDING}, 
						 {duplicate_channel, ?SERVER_ERROR_DUPLICATE_CHANNEL},
						 {no_records, ?SERVER_ERROR_RECORD_NOT_FOUND},
						 {no_admin_key, ?SERVER_ERROR_ADMIN_KEY},
						 {no_action_specified, ?SERVER_ERROR_NO_ACTION_SPECIFIED},
						 {missing_action_type, ?SERVER_ERROR_ACTION_TYPE},
						 {no_such_client, ?SERVER_ERROR_NO_SUCH_CLIENT},
						 {invalid_parameters, ?SERVER_ERROR_INVALID_PARAMETER},
						 {channel_id_required, ?SERVER_ERROR_MISSING_CHANNEL},
						 {not_implemented, ?SERVER_ERROR_NOT_IMPLEMENTED},
						 {client_id_required, ?SERVER_ERROR_MISSING_CLIENT},
						 {no_such_channel, ?SERVER_ERROR_NO_SUCH_CHANNEL},
						 {no_such_command, ?SERVER_ERROR_NO_SUCH_COMMAND},
						 {no_such_link, ?SERVER_ERROR_LINK_NOT_FOUND},
						 {target_client_id_required, ?SERVER_ERROR_TARGET_ID_REQUIRED},
						 {channel_data_required, ?SERVER_ERROR_CHANNEL_DATA_REQUIRED},
						 {invalid_channel_data, ?SERVER_ERROR_INVALID_CHANNEL_DATA},
						 {video_link_required, ?SERVER_ERROR_VIDEO_LINK_REQUIRED},
 						 {no_such_endpoint, ?SERVER_ERROR_NO_SUCH_ENDPOINT},
 						 {missing_user_id, ?SERVER_ERROR_USER_ID},
 						 {missing_password, ?SERVER_ERROR_USER_PASSWORD}]).

%%% List of tuples, such that the first member is the query command. The second member is the query value
-define(CELESTIAL_OBJECTS, [{mercury, {keywords, [<<"mercury">>]}}, 
							{venus, {keywords, [<<"venus">>]}}, 
							{mars, {keywords, [<<"mars">>]}}, 
							{jupiter, {keywords,[<<"jupiter">>]}}, 
							{saturn, {keywords,[<<"saturn">>]}}, 
							{urnaus, {keywords,[<<"uranus">>]}}, 
							{neptune, {keywords,[<<"neptune">>]}}, 
							{pluto, {keywords,[<<"pluto">>]}}, 
							{europa, {keywords,[<<"europa">>]}}, 
							{ganymede, {keywords,[<<"ganymede">>]}}, 
							{io, {keywords,[<<"io">>]}}, 
							{callisto, {keywords,[<<"callisto">>]}}, 
							{titan, {keywords,[<<"titan">>]}},
							{enceladus, {keywords,[<<"enceladus">>]}},
							{dione, {keywords,[<<"dione">>]}},
							{iapetus, {keywords,[<<"iapetus">>]}},
							{mimas, {keywords,[<<"mimas">>]}},
							{tethys, {keywords,[<<"tethys">>]}},
							{hyperion, {keywords,[<<"hyperion">>]}},
							{epimetheus, {keywords,[<<"epimetheus">>]}},
							{phoebe, {keywords,[<<"phoebe">>]}},
							{rhea, {keywords,[<<"rhea">>]}},
							{galaxies,{keywords, [<<"galaxies,">>, <<"galaxy,">>, <<"nebula">>]}},
							{galaxy, {keywords,[<<"galaxy">>]}},
							{exoplanet, {keywords,[<<"exoplanet">>]}},
							{sun, {keywords,[<<"Solar Dynamics Observatory">>]}}, 
							{chandra, {keywords,[<<"Chandra X-ray Observatory">>]}} %%% Use keywords="Chandra X-ray Observatory"
							]). 
-define(CHILD_SPEC_1(CELSESTIALOBJECT, DATESTART, DATEEND), 
			{serv2, 
				{nasa_data_aquisition_server, start_link, [CELSESTIALOBJECT, DATESTART,DATEEND]},
				temporary, 1000, worker,[simple_one_for_one]
			}
        ).

%%% A list of tuples. The first member is the Json tag to retrieve. The second member is a list of tuples that marks the json entry as unusable.
-define(EXCLUTION_LIST, [ 
		{title, [<<"animation">>, <<"venus transit">>]},
		{center, [<<"arc">>, <<"hq">>, <<"ksc">>, <<"msfc">>, <<"jrc">> ,<<"grc">>, <<"jsc">>, <<"ssc">>]},
		{keywords, [<<"engage series">>, <<"president">>, <<"humans in space">>, <<"international space station">>, <<"pressconference">>]},
		{nasa_id, [<<"pia11404">>,<<"pia11219">>,<<"pia14083">>,<<"pia14079">>,	<<"pia17385">>,<<"pia12083">>,<<"pia14279">>,<<"pia12366">>,<<"pia12278">>,<<"pia11078">>,<<"pia10396">>,<<"pia10395">>,
					<<"pia14358">>,<<"pia11414">>,<<"pia16866">>,<<"pia11372">>,<<"pia11959">>,<<"pia14078">>,<<"pia12364">>,<<"pia14078">>,<<"pia14078">>,<<"pia12213">>,<<"pia10189">>,<<"pia10380">>,
					<<"pia03101">>,<<"pia11076">>,<<"pia16951">>,<<"pia12369">>,<<"pia16525">>,<<"pia16539">>,<<"pia16823">>,<<"pia13188">>,<<"pia13188">>]},
				{description, [<<"northrop">>,<<"anniversary">>,<<"award">>,<<"administrator">>,<<"aboard the rocket">>,<<"animation">>,<<"begins assembly">>,<<"building">>,<<"chair of">>,<<"control room">>,
						<<"cleanroom">>,<<"clean room">>,<<"engineers">>,<<"engineer">>,<<"employees">>,<<"enjoying">>,<<"fabricated">>,<<"goddard space flight center">>,<<"graphic">>,<<"hubble opperations">>,
						<<"illustration">>,<<"model of">>,<<"operates">>,<<"process of assembly">>,<<"propulsion module">>,<<"short clip">>,<<"supercomputer">>,<<"speakers">>,<<"science instruments">>,<<"laser altimeter">>,
						<<"altimetry">>,<<"movie">>,<<"movies">>,<<"video">>,<<"3-d">>,<<"3d">>,<<"girl">>,<<"woman">>,<<"women">>,<<"payload">>,<<"payloads">>]}
	]).