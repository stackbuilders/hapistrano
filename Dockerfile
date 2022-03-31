FROM alpine:3.15 AS build
RUN apk update && \
    apk add \
      alpine-sdk \
      bash \
      ca-certificates \
      cabal \
      ghc \
      ghc-dev \
      git \
      gmp-dev \
      gnupg \
      libffi-dev \
      linux-headers \
      zlib-dev
WORKDIR /usr/src/app
COPY hapistrano.cabal .
RUN cabal update && \
    cabal configure -f static && \
    cabal build --only-dependencies
COPY . .
RUN cabal install

FROM alpine:3.15
MAINTAINER Nicolas Vivar <nvivar@stackbuilders.com>
RUN apk update && \
    apk add \
      ca-certificates \
      git \
      openssh-client
RUN mkdir ~/.ssh
COPY --from=build /root/.cabal/bin/hap /usr/local/bin/hap
ENTRYPOINT ["/usr/local/bin/hap"]
