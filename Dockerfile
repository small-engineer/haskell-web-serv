FROM haskell:9.8 as build

WORKDIR /app

COPY stack.yaml stack.yaml.lock mnist-web.cabal Setup.hs ./
COPY src ./src

RUN stack setup \
    && stack build --copy-bins --local-bin-path /app/bin

FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates libgmp10 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/bin/mnist-web /usr/local/bin/mnist-web

ENV JWT_SECRET="euwgbwigbewiguwebgiwuebgiweubwigu"
EXPOSE 8080

CMD ["mnist-web"]
