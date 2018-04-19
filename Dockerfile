# Build Hapistrano
FROM fpco/stack-build as build-env

MAINTAINER Javier Casas <jcasas@stackbuilders.com>

WORKDIR /hapistrano
COPY stack.yaml .
COPY hapistrano.cabal .
RUN stack build --only-dependencies

COPY src/ src/
COPY app/ app/
COPY script/ script/
COPY LICENSE .
COPY Setup.hs .
RUN stack install

# Copy Hapistrano to a basic Debian
FROM debian:stretch-slim
RUN apt-get update && \
    apt-get install -y libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build-env /root/.local/bin/hap /bin

# To ensure it works on Debian
RUN hap --version
CMD /bin/hap
