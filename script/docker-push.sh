#!/bin/bash
function log {
  echo -e "\e[32m${1}\e[0m"
}

log "Authenticating with Docker"
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin

log "Building Hapistrano's Image"
docker build . -t hapistrano

log "Removing Container"
docker run --rm hapistrano --version
