# Build Hapistrano
FROM alpine:3.9 as build-env

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
# So Hapistrano is built with version information
COPY .git/ .git/

RUN cabal configure -f static
RUN cabal build hap

# Compress the resulting binary
RUN upx /hapistrano/dist/build/hap/hap

# Copy Hapistrano to a basic Alpine with SSH
FROM alpine:3.9

RUN apk update \
 && apk add \
        openssh-client \
        ca-certificates \
        git

RUN mkdir ~/.ssh

COPY --from=build-env /hapistrano/dist/build/hap/hap /bin/hap

ENTRYPOINT ["/bin/hap"]
