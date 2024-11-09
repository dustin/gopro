# Stage 1: Build the Haskell project
FROM haskell:9.6-bullseye AS builder

RUN apt-get update && apt-get install -y ffmpeg libpq5 libpq-dev zlib1g zlib1g-dev

WORKDIR /usr/src/app

COPY gopro.cabal ./
RUN cabal update && cabal build --only-dependencies

COPY . ./
RUN cabal install

# Stage 2: Create the final image
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y ffmpeg libpq5 zlib1g ca-certificates

WORKDIR /usr/local/bin

# Copy the built executable from the builder stage
COPY --from=builder /root/.local/bin/gopro .

VOLUME /data

# Copy the static directory
WORKDIR /data
COPY --from=builder /usr/src/app/static /data/static

ENTRYPOINT ["/usr/local/bin/gopro"]
