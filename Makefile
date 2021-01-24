.PHONEY: all stop ps

all:
	cd webserver ; make all
	cd apserver ; make all

stop:
	cd webserver ; make stop
	cd apserver ; make stop

ps:
	ps aux | grep erlang | grep -v grep || echo 'no process'
