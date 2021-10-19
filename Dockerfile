# Build Hapistrano
FROM alpine:3.13 as build-env

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
COPY src/ src/
COPY app/ app/
COPY script/ script/
COPY LICENSE .
COPY Setup.hs .
# So Hapistrano is built with version information
COPY .git/ .git/
# Cabal has changed behaviour and it requires all modules listed
COPY spec/ spec/
COPY fixtures/ fixtures/
COPY CHANGELOG.md .
COPY README.md .
RUN touch Dockerfile

RUN cabal update
RUN cabal install --only-dependencies
RUN cabal configure -f static
RUN cabal build hap

# Compress the resulting binary
RUN mkdir bin
RUN cp /hapistrano/dist-newstyle/build/x86_64-linux/ghc-8.8.4/hapistrano-0.4.3.0/x/hap/build/hap/hap bin/
RUN upx /hapistrano/bin/hap

# Copy Hapistrano to a basic Alpine with SSH
FROM alpine:3.13

RUN apk update \
 && apk add \
        openssh-client \
        ca-certificates \
        git

RUN mkdir ~/.ssh

COPY --from=build-env /hapistrano/bin/hap /bin/hap

ENTRYPOINT ["/bin/hap"]
