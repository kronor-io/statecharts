FROM postgres:13

RUN apt-get -qq update \
    && apt-get -qq --no-install-recommends install less libperl5.28 perl-doc nano ca-certificates git \
       libpq5 postgresql-client \
       libtap-parser-sourcehandler-pgtap-perl postgresql-13-pgtap \
       pgxnclient build-essential postgresql-server-dev-13 \
       sqitch \
    && apt-cache pkgnames | grep libmagic | xargs apt-get purge -qq \
    && apt-get clean \
    # Let libcurl find certs. https://stackoverflow.com/q/3160909/79202
    && mkdir -p /etc/pki/tls && ln -s /etc/ssl/certs /etc/pki/tls/ \
    && rm -rf /var/cache/apt/* /var/lib/apt/lists/*

RUN pgxn install semver

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
