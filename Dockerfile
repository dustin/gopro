FROM haskell:9.2.8-buster

RUN apt-get update
RUN apt-get install -y ffmpeg libpq5 libpq-dev zlib1g zlib1g-dev

WORKDIR /usr/lib/gopro
COPY stack.yaml package.yaml ./
RUN stack install --system-ghc --dependencies-only

COPY . ./
RUN stack install --system-ghc

CMD ["/root/.local/bin/gopro"]
