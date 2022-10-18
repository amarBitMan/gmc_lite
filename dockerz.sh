#!/usr/bin/env bash

if [[ $1 = "build" ]] ; then
   docker build -t local/gmc_lite:latest -f Dockerfile .
elif [[ $1 = "run" ]] ; then
    docker stop gmc_lite
    docker rm gmc_lite
    docker run --name gmc_lite -dp 9011:9011 --mount type=bind,source="$(pwd)"/data,target=/opt/gmc_lite/data local/gmc_lite
elif [[ $1 = "attach" ]] ; then
    docker exec -it gmc_lite sh
else
    echo "Usage: $(basename "$0") arg"
    echo "arg=build:    To build a docker image"
    echo "arg=run:      To run a docker container"
    echo "arg=attach:   To access shell of docker container"
fi
