# Build Hapistrano
FROM fpco/stack-build as build-env

MAINTAINER Javier Casas <jcasas@stackbuilders.com>

WORKDIR /hapistrano
COPY hapistrano.cabal .
RUN cabal update
RUN cabal install --only-dependencies
COPY src/ src/
COPY app/ app/
COPY script/ script/
COPY LICENSE .
COPY Setup.hs .
RUN cabal build hapistrano
RUN cabal run -- --version
RUN find / | grep hap
RUN /hapistrano/dist/build/hap/hap --version


# Copy Hapistrano to a basic Debian
FROM debian:stretch-slim
RUN apt-get update && \
    apt-get install -y libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build-env /hapistrano/dist/build/hap/hap /bin

RUN hap --version

ENTRYPOINT /bin/hap
