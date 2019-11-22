# Build Hapistrano
FROM alpine:3.9 as build-env

MAINTAINER Nicolas Vivar <nvivar@stackbuilders.com>

RUN apk update \
 && apk add \
        alpine-sdk \
        bash \
        ca-certificates \
        cabal \
        ghc-dev \
        ghc \
        git \
        gmp-dev \
        gnupg \
        libffi-dev \
        linux-headers \
        upx \
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
