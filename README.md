# Browser game - Concentration

This repository implements the card game
[Concentration](https://en.wikipedia.org/wiki/Concentration_(card_game)) as a browser game.

**THIS GAME IS NOT COMPLETED YET.**

## Quick start

First, you can build and start a web server.
And you need to enter your PC password at this time (This reason is described later).

~~~bash
cd webserver
make all
~~~

Second, you can build and start an AP server.

~~~bash
cd apserver
make all
~~~

Finally, open http://localhost in your browser, then let's play Concentration!

## Notes

The web server listens to port 80 on your PC.
So you need to enter your PC password at starting this server.

Also, the AP server listens to port 8080 on your PC.
