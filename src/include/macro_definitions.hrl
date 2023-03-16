-define(NASA_IMAGES_HOST, "https://images-api.nasa.gov/search?").
-define(APOD_HOST, "https://api.nasa.gov/planetary/apod?").
-define(YOUTUBE_HOST, "https://www.googleapis.com/youtube/v3/search?").
-define(ASTRONOMY_API_KEY, "K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8").
-define(YOUTUBE_API_KEY, "AIzaSyBWhaM01ueTAyMIhWf11zbJBo3ulKA89u4").
-define(YOUTUBE_CHANNEL_IDS, [<<"UC7_gcs09iThXybpVgjHZ_7g">>, <<"UCQfZkf3-Y2RwzdRFWXYsdaQ">>]).
-define(YOUTUBE_CLIENT_KEY, "389b4e36-b3d2-11ed-afa1-0242ac120002").
-define(ADMINISTRATOR_KEY, "f09a2270-ac19-418c-a443-9f9e4f4c9019").
-define(YOUTUBE_MAXRESULTS, "50").
-define(FIRST_PUBLISH_DATE, "1990-01-01T00:00:00Z").
-define(SERVER_CONFIG_FILE, <<"server_config.cfg">>).

-define(YOUTUBE_NEXTPAGE, <<"nextPageToken">>).
-define(TOP_KEY, <<"Videos">>).
-define(YOUTUBE_VIDEO_ARRAY_KEY, <<"items">>).
-define(YOUTUBE_RETURN_VIEDO_LIST_KEY, <<"Videos">>).
-define(ADMIN_AVAILABLE_ACTIONS, [<<"delete">>, <<add>>]).
-define(YOUTUBE_KEY, <<"youtubekey">>).

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