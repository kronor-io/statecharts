FROM ubuntu:21.04

RUN apt-get update \
 && apt-get install -y --no-install-recommends curl libnuma-dev libpq-dev ca-certificates

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /app


COPY ./stack.yaml /app/
COPY ./stack.yaml.lock /app/
COPY ./package.yaml /app/

RUN export PATH="$HOME"/.local/bin:"$PATH" \
 && stack build --only-dependencies
