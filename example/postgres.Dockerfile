FROM postgres:18

# sqitch to run the migrations, plus the PGXN client to fetch the extensions
# and make for it to build them with. PGXS already ships inside the server
# image, and because pg_statecharts is pure SQL there is still no compiler and
# no server headers here. ltree is the only extension dependency and it ships
# with PostgreSQL itself.
RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install sqitch pgxnclient make \
  && rm -rf /var/lib/apt/lists/*

# Both extensions come from PGXN, which works out the latest stable release
# itself. Nothing here is tied to a version or to a PostgreSQL major: the
# distributions are pure SQL and the same files install on any server.
#
# The runtime goes first because pgxnclient does not follow the dependency
# that pg_statecharts_dev declares in its META.json.
#
# pg_statecharts_dev is the half that reads and writes files on the database
# host; it is here because this example generates migrations from .scxml
# files. A production image would install pg_statecharts only.
RUN pgxn install pg_statecharts \
  && pgxn install pg_statecharts_dev
