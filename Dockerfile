FROM haskell:9.10.3 AS build

WORKDIR /app

COPY stack.yaml stack.yaml.lock mnist-web.cabal Setup.hs ./
COPY src ./src

RUN stack --install-ghc setup \
    && stack --install-ghc build --copy-bins --local-bin-path /app/bin

FROM debian:12-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/bin/mnist-web /usr/local/bin/mnist-web

EXPOSE 8080

CMD ["mnist-web"]
