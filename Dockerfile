# Build Hapistrano
FROM alpine:3.7 as build-env

MAINTAINER Javier Casas <jcasas@stackbuilders.com>

RUN echo '@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories
RUN apk update \
 && apk add \
        alpine-sdk \
        bash \
        ca-certificates \
        cabal@testing \
        ghc-dev@testing \
        ghc@testing \
        git \
        gmp-dev \
        gnupg \
        libffi-dev \
        linux-headers \
        upx@testing \
        zlib-dev

WORKDIR /hapistrano

COPY hapistrano.cabal .

RUN cabal update
RUN cabal install --only-dependencies

COPY src/ src/
COPY app/ app/
COPY script/ script/
COPY LICENSE .
COPY Setup.hs .

RUN cabal configure -f static
RUN cabal build hap

# Compress the resulting binary
RUN upx /hapistrano/dist/build/hap/hap

# Copy Hapistrano to a basic Alpine
FROM alpine:3.7

COPY --from=build-env /hapistrano/dist/build/hap/hap /bin/hap

ENTRYPOINT ["/bin/hap"]
