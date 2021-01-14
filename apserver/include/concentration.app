{application, concentration,
			  [{description, "Browser game - Concentration"},
			   {vsn, "1.0"},
			   {modules, [concentration_app, room_server, matching_list_server, providing_deck_server, concentration_supervisor]},
			   {registered, [room_server, matching_list_server, concentration_supervisor]},
			   {applications, [kernel, stdlib]},
			   {mod, {concentration_app, []}},
			   {start_pases, []}
			  ]}.
