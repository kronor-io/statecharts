FROM postgres:18

# sqitch to run the migrations, plus curl and jq to fetch the extension
# packages from GitHub. That is the whole build environment: because
# pg_statecharts is pure SQL there is no compiler, no PGXN client and no
# server headers here. ltree is the only extension dependency and it ships
# with PostgreSQL itself.
RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install sqitch curl ca-certificates jq \
  && rm -rf /var/lib/apt/lists/*

# Install both extensions from the latest GitHub release. The .deb packages are
# named per PostgreSQL major version, because that is the only thing that
# varies -- the files inside are architecture independent SQL -- so the major
# version comes from pg_config rather than being hard coded to the FROM above.
#
# pg-statecharts-dev is the half that reads and writes files on the database
# host; it is here because this example generates migrations from .scxml files.
# A production image would install pg-statecharts only.
#
# dpkg -i rather than apt: the only dependencies are postgresql-18, already in
# this image, and the runtime package, installed by the first pass of the loop.
RUN set -eux; \
  PG_VERSION=$(pg_config --version | sed -n 's/^PostgreSQL \([0-9]*\).*/\1/p'); \
  for EXTENSION_NAME in pg-statecharts pg-statecharts-dev; do \
    RELEASE_NAME_PATTERN="${EXTENSION_NAME}-${PG_VERSION}_"; \
    JQ_QUERY='.assets[] | select(.name | startswith($PATTERN)) | .browser_download_url'; \
    DOWNLOAD_URL=$(curl -sS https://api.github.com/repos/kronor-io/statecharts/releases/latest \
      | jq -r --arg PATTERN "$RELEASE_NAME_PATTERN" "$JQ_QUERY"); \
    curl -fsSL -o /tmp/extension.deb "$DOWNLOAD_URL"; \
    dpkg -i /tmp/extension.deb; \
    rm /tmp/extension.deb; \
  done
