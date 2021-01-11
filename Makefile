.PHONEY: ps

ps:
	ps aux | grep erlang | grep -v grep || echo 'no process'
