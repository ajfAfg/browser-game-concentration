#!/bin/bash

curl -X POST -d 'user_id=1' localhost:8080/room &
matching_id=`curl -X POST -d 'user_id=2' localhost:8080/room`

sleep 1

curl -X POST -d 'user_id=1' -d "matching_id=${matching_id}" localhost:8080/dealer
curl -X POST -d 'user_id=2' -d "matching_id=${matching_id}" localhost:8080/dealer

sleep 1

curl -X POST -d 'user_id=1' -d "matching_id=${matching_id}" localhost:8080/match &
curl -X POST -d 'user_id=2' -d "matching_id=${matching_id}" -d 'x=1' -d 'y=2' localhost:8080/match
