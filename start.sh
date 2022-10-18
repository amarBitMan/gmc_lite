#!/usr/bin/env bash

set -xe

# docker build -t amar/gmc_lite:latest -f Dockerfile .
# docker run --name gmc_lite -dp 9011:9011  amar/gmc_lite

docker run --name gmc_lite -dp 9011:9011 --mount type=bind,source="$(pwd)"/data,target=/opt/gmc_lite/data amar/gmc_lite