FROM quay.io/benz0li/ghc-musl:9.6.6 AS build
WORKDIR /usr/src/app
COPY hapistrano.cabal .
RUN cabal update && \
    cabal build --only-dependencies --enable-static
COPY . .
RUN cabal build --enable-executable-static && \
    cp $(cabal exec which hap) hap

FROM alpine:3.15
MAINTAINER Cristhian Motoche <cmotoche@stackbuilders.com>
RUN apk update && \
    apk add \
      ca-certificates \
      git \
      openssh-client
RUN mkdir ~/.ssh
COPY --from=build /usr/src/app/hap /usr/local/bin/hap
ENTRYPOINT ["/usr/local/bin/hap"]
CMD ["--help"]
