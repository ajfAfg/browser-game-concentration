# Browser game - Concentration

This repository implements the card game
[Concentration](https://en.wikipedia.org/wiki/Concentration_(card_game)) as a browser game.

**THIS GAME IS NOT COMPLETED YET.**

## Quick start

You can build and start a Web server and an AP server.
Note that you need to enter your PC password at this time (This reason is described later).

~~~bash
$ pwd
/path/to/browser-game-concentration
$ make all
~~~

And open http://localhost in your browser, then let's play Concentration!

## Notes

The web server listens to port 80 on your PC.
So you need to enter your PC password at starting this server.

Also, the AP server listens to port 8080 on your PC.

## Cautions

You might fail when use the make `target` start in the directory webserver.
Please use the make target `start2` instead of it at that time.
