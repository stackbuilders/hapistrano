#!/usr/bin/env bash
set -e
set -o pipefail
echo $DOCKER_HUB_PASSWORD | docker login -u $DOCKER_HUB_USERNAME --password-stdin
docker push stackbuilders/hapistrano:$(cat hapistrano.cabal | grep ^version: | awk '{print $2}')
