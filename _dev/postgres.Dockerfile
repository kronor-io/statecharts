FROM postgres:15

RUN apt-get -qq update \
    && apt-get -qq --no-install-recommends install \
    # required by squitch
    less "libperl5.*" perl-doc nano ca-certificates git \
    libpq5 postgresql-client-15 \
    # required by pgtap
    libtap-parser-sourcehandler-pgtap-perl postgresql-15-pgtap \
    # required by plpgsql_check
    libicu-dev \
    # required by pgxn
    pgxnclient build-essential postgresql-server-dev-15 \
    sqitch \
    && apt-cache pkgnames | grep libmagic | xargs apt-get purge -qq \
    && apt-get clean \
    && rm -rf /var/cache/apt/* /var/lib/apt/lists/*

RUN pgxn install semver \
    && pgxn install plpgsql_check

COPY _dev/init-extensions.sh /docker-entrypoint-initdb.d/init-extensions.sh

ENV LESS=-R \
    LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    SQITCH_PAGER=less \
    POSTGRES_HOST_AUTH_METHOD=trust \
    POSTGRES_USER=statecharts \
    PGUSER=statecharts \
    POSTGRES_DB=statecharts \
    PGDATABASE=statecharts
WORKDIR /repo
