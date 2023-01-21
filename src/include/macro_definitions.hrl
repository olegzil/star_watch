-define(NASA_IMAGES_HOST, "https://images-api.nasa.gov/search?").
-define(APOD_HOST, "https://api.nasa.gov/planetary/apod?").
-define(API_KEY, "K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8").
%%% List of tuples, such that the first member is the query command. The second member is the query value
-define(CELESTIAL_OBJECTS, [{q, <<"mercury">>}, 
							{q, <<"venus">>}, 
							{q, <<"mars">>}, 
							{q,<<"jupiter">>}, 
							{q,<<"saturn">>}, 
							{q,<<"uranus">>}, 
							{q,<<"neptune">>}, 
							{q,<<"pluto">>}, 
							{q,<<"europa">>}, 
							{q,<<"ganymede">>}, 
							{q,<<"io">>}, 
							{q,<<"callisto">>}, 
							{q,<<"titan">>},
							{q,<<"enceladus">>},
							{q,<<"dione">>},
							{q,<<"iapetus">>},
							{q,<<"mimas">>},
							{q,<<"tethys">>},
							{q,<<"hyperion">>},
							{q,<<"epimetheus">>},
							{q,<<"phoebe">>},
							{q,<<"rhea">>},
							{q,<<"black hole">>},
							{q,<<"galaxy">>},
							{q,<<"exoplanet">>},
							{q,<<"deep field">>},
							{q,<<"dark matter">>},
							{center, <<"jpl">>},
							{center, <<"STScI">>},
							{keywords,<<"sun">>}, 
							{keywords,<<"Chandra X-ray Observatory">>} %%% Use keywords="Chandra X-ray Observatory"
							]). 
-define(CHILD_SPEC_1(CELSESTIALOBJECT, DATESTART, DATEEND), 
			{serv2, 
				{nasa_data_aquisition_server, start_link, [CELSESTIALOBJECT, DATESTART,DATEEND]},
				temporary, 1000, worker,[simple_one_for_one]
			}
        ).

%%% A list of tuples. The first member is the Json tag to retrieve. The second member is a list of tuples that marks the json entry as unusable.
-define(EXCLUTION_LIST, [ 
		{description, [<<"northrop">>,
						<<"anniversary">>,
						<<"award">>,
						<<"Administrator">>,
						<<"speakers">>,
						<<"chair of">>,
						<<"short clip">>,
						<<"Hubble Opperations">>,
						<<"Goddard Space Flight Center">>,
						<<"control room">>,
						<<"Supercomputer">>,
						<<"   ">>]},
		{center, [<<"ARC">>, <<"HQ">>, <<"KSC">>, <<"MSFC">>, <<"JRC">>, <<"SSC">>]},
		{keywords, [<<"Engage series">>, <<"President">>]}
	]).
