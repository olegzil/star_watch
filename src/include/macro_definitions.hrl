-define(NASA_IMAGES_HOST, "https://images-api.nasa.gov/search?").
-define(APOD_HOST, "https://api.nasa.gov/planetary/apod?").
-define(YOUTUBE_HOST, "https://www.googleapis.com/youtube/v3/search?").
-define(ASTRONOMY_API_KEY, "K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8").
-define(YOUTUBE_CHANNEL_IDS, [<<"UC7_gcs09iThXybpVgjHZ_7g">>, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>]).
-define(CLIENT_ACCESS_KEY, <<"389b4e36-b3d2-11ed-afa1-0242ac120002">>).
-define(ADMINISTRATOR_KEY, <<"f09a2270-ac19-418c-a443-9f9e4f4c9019">>).
-define(YOUTUBE_MAXRESULTS, "50").
-define(FIRST_PUBLISH_DATE, "1990-01-01T00:00:00Z").
-define(SERVER_CONFIG_FILE, <<"server_config.cfg">>).

-define(YOUTUBE_NEXTPAGE, <<"nextPageToken">>).
-define(TOP_KEY, <<"Videos">>).
-define(YOUTUBE_VIDEO_ARRAY_KEY, <<"items">>).
-define(YOUTUBE_RETURN_VIEDO_LIST_KEY, <<"Videos">>).
-define(ADMIN_AVAILABLE_ACTIONS, [<<"delete">>, <<add>>]).
-define(YOUTUBE_KEY, <<"youtubekey">>).
-define(REQUIRED_CLIENT_KEY_TOKEN, <<"key">>).
-define(REQUIRED_ACTION_TOKEN, <<"action">>).
-define(REQUIRED_CHANNEL_ID_TOKEN, <<"channel_id">>).
-define(REQUIRED_CLIENT_ID_TOKEN, <<"client_id">>).
-define(AVAILABLE_CHANNEL_ACTIONS, [<<"fetchclientdirectory">>,<<"fetchchannelvideos">>,<<"updatechannel">>,<<"addvideolink">>, <<"linkstatus">>, <<"fetchclientprofile">>, <<"updateclientprofile">>, <<"fetchchannelimage">>]).

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

-define(RESPONSE_CODES, [{video_link_added, ?SERVER_ERROR_LINK_ADDED},
						 {link_exists, ?SERVER_ERROR_LINK_EXISTS}, 
						 {link_pending, ?SERVER_ERROR_LINK_PENDING}, 
						 {video_link_added, ?SERVER_ERROR_OK}, 
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
						 {no_such_link, ?SERVER_ERROR_LINK_NOT_FOUND}]).

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