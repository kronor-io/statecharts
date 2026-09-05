FROM postgres:18

# sqitch to run the migrations. That is the whole build environment: because
# pg_statecharts is pure SQL there is no compiler, no PGXN client and no
# server headers here. ltree is the only extension dependency and it ships
# with PostgreSQL itself.
RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install sqitch \
  && rm -rf /var/lib/apt/lists/*

# Installing the extension is copying two files per extension into the
# extension directory. They are plain SQL, so the same files work on every
# PostgreSQL version, architecture and operating system.
#
# This copies them straight out of the checkout. To use a released version
# instead, download and unpack the release tarball and run its install.sh.
COPY pg_statecharts/pg_statecharts.control /tmp/ext/pg_statecharts/
COPY pg_statecharts/sql /tmp/ext/pg_statecharts/sql
COPY pg_statecharts/install.sh /tmp/ext/pg_statecharts/
COPY pg_statecharts_dev/pg_statecharts_dev.control /tmp/ext/pg_statecharts_dev/
COPY pg_statecharts_dev/sql /tmp/ext/pg_statecharts_dev/sql
COPY pg_statecharts_dev/install.sh /tmp/ext/pg_statecharts_dev/

RUN /tmp/ext/pg_statecharts/install.sh \
  && /tmp/ext/pg_statecharts_dev/install.sh \
  && rm -rf /tmp/ext
