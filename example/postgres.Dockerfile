FROM postgres:18

RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install \
  sqitch \
  jq \
  # needed by the semver extension
  make gcc postgresql-server-dev-18 \
  pgxnclient \
  curl ca-certificates

RUN pgxn install semver

RUN \
  set -ex; \
  PG_VERSION=$(pg_config --version | sed -n 's/^PostgreSQL \([0-9]*\).*/\1/p'); \
  RELEASE_NAME_PATTERN="pg${PG_VERSION}_$(uname -s)_$(uname -m).deb"; \
  JQ_QUERY='sort_by(.published_at) | reverse | .[0] | .assets[] | select(.name | endswith($PATTERN)) | .browser_download_url';  \
  DOWNLOAD_URL=$(curl -s https://api.github.com/repos/kronor-io/statecharts/releases | jq -r --arg PATTERN "$RELEASE_NAME_PATTERN" "$JQ_QUERY"); \
  EXTENSION_PATH=/tmp/extension.deb; \
  curl -L -o $EXTENSION_PATH "$DOWNLOAD_URL"; \
  apt install $EXTENSION_PATH; \
  rm $EXTENSION_PATH;

