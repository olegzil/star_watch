-define(NASA_IMAGES_HOST, "https://images-api.nasa.gov/search?").
-define(APOD_HOST, "https://api.nasa.gov/planetary/apod?").
-define(API_KEY, "K9jqPfqphwz3s1BsTbPQjsi2c4kn4eV7wBFh2MR8").
-define(CELESTIAL_OBJECTS, [<<"mercury">>, 
							<<"venus">>, 
							<<"mars">>, 
							<<"jupiter">>, 
							<<"saturn">>, 
							<<"uranus">>, 
							<<"neptune">>, 
							<<"pluto">>, 
							<<"europa">>, 
							<<"ganymede">>, 
							<<"io">>, 
							<<"calisto">>, 
							<<"titan">>,
							<<"enceladus">>,
							<<"dione">>,
							<<"iapetus">>,
							<<"mimas">>,
							<<"tethys">>,
							<<"hyperion">>,
							<<"epimetheus">>,
							<<"phoebe">>,
							<<"rhea">>,
							<<"black_hole">>,
							<<"galaxy">>,
							<<"exoplanet">>,
							<<"deep_field">>]).
-define(CHILD_SPEC_1(CELSESTIALOBJECT, DATESTART, DATEEND), 
			{serv2, 
				{nasa_data_aquisition, start_link, [CELSESTIALOBJECT, DATESTART,DATEEND]},
				temporary, 1000, worker,[simple_one_for_one]
			}
        ).