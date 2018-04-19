# Build Hapistrano
FROM fpco/stack-build as build-env

MAINTAINER Javier Casas <jcasas@stackbuilders.com>

WORKDIR /hapistrano
COPY . .
RUN stack install

# Copy Hapistrano to a basic Ubuntu
FROM ubuntu:16.04
RUN apt-get update && \
    apt-get install -y libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build-env /root/.local/bin/hap /bin

# To ensure it works on Ubuntu
RUN hap --help
