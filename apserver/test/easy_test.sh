#!/bin/bash

curl -X POST -d 'user_id=1' localhost:8080/room &
matching_id=`curl -X POST -d 'user_id=2' localhost:8080/room`

sleep 1

curl -X POST -d 'user_id=1' -d "matching_id=${matching_id}" localhost:8080/deck &
curl -X POST -d 'user_id=2' -d "matching_id=${matching_id}" localhost:8080/deck &

sleep 1

curl -X POST -d 'user_id=1' -d "matching_id=${matching_id}" localhost:8080/first_player &
curl -X POST -d 'user_id=2' -d "matching_id=${matching_id}" localhost:8080/first_player &

sleep 1


curl -X POST -d 'user_id=2' -d "matching_id=${matching_id}" -d 'turn=1' -d 'x=1' -d 'y=2' localhost:8080/match &
curl -X POST -d 'user_id=1' -d "matching_id=${matching_id}" -d 'turn=1' localhost:8080/match &
