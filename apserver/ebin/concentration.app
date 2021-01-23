{application, concentration,
			  [{description, "Browser game - Concentration"},
			   {vsn, "1.0"},
			   {modules, [concentration_app, room_server, matching_list_server, providing_deck_server, concentration_supervisor, match_supervisor, turn_player, non_turn_player, match_move_dealer]},
			   {registered, []},
			   {applications, [kernel, stdlib]},
			   {mod, {concentration_app, []}},
			   {start_pases, []}
			  ]}.
