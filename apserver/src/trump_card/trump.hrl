-export_type([trump/0]).

-record(trump,
		{suit :: suit:suit(),
		 number :: trump_number:trump_number()
		}).

-opaque trump() :: #trump{}.
