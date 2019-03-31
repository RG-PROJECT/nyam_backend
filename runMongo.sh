#!/usr/bin/env bash

docker run --name mongo -p 27017:27017 -d mongo
docker exec -it mongo /bin/bash
