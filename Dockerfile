# Build
FROM quay.io/benz0li/ghc-musl:9.6.6 AS build

WORKDIR /usr/src/app

# Install upx
RUN apk update && apk add --no-cache upx

# Copy only the necessary files for dependency installation
COPY hapistrano.cabal ./

# Install dependencies
RUN cabal update && \
    cabal build --only-dependencies --enable-static

# Copy the rest of the files.
COPY . .

# Build the application and compress the binary
RUN cabal build --enable-executable-static && \
    cp $(cabal exec which hap) hap && \
    upx hap
# Final image
FROM alpine:3.15

LABEL maintainer="Cristhian Motoche <cmotoche@stackbuilders.com>"

# Install runtime dependencies
RUN apk update && \
    apk add --no-cache \
      ca-certificates \
      git \
      openssh-client

# Create .ssh directory
RUN mkdir -p ~/.ssh

# Copy the binary from the build stage
COPY --from=build /usr/src/app/hap /usr/local/bin/hap

# Set the entrypoint and default command
ENTRYPOINT ["/usr/local/bin/hap"]
CMD ["--help"]
