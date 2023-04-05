FROM utdemir/ghc-musl:v24-ghc902 AS build
WORKDIR /usr/src/app
COPY hapistrano.cabal .
RUN cabal update && \
    cabal build --only-dependencies --enable-static
COPY . .
RUN cabal build --enable-executable-static && \
    cp $(cabal exec which hap) hap

FROM gcr.io/distroless/static-debian11
LABEL maintainer="Nicolas Vivar <nvivar@stackbuilders.com>"
COPY --from=build /usr/src/app/hap /usr/local/bin/hap
ENTRYPOINT ["/usr/local/bin/hap"]
