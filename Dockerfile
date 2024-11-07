FROM haskell:9.6-bullseye

RUN apt-get update
RUN apt-get install -y ffmpeg libpq5 libpq-dev zlib1g zlib1g-dev

WORKDIR /usr/lib/gopro

COPY gopro.cabal ./
RUN cabal update
RUN cabal build --only-dependencies

COPY . ./
RUN cabal install

ENTRYPOINT ["/root/.local/bin/gopro"]
