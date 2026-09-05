# Example

A tiny lightswitch statechart. There is a `lightswitch` table that records when
the switch was turned on and off, and a state machine with two states, `on` and
`off`, that drives it.

Everything runs in Docker, and the database container runs **as you** rather
than as its own postgres user. That matters because the migration generator
writes files from inside the container: run it as postgres and the generated
migrations land in your repository owned by uid 999, which you cannot then edit
or commit.

## Setup

```bash
make up                    # writes .env with your uid/gid and sqitch identity, then starts postgres
make deploy-migrations     # creates the extension and the lightswitch table
```

The first migration contains `create extension pg_statecharts cascade`, so it
has to run before any statechart functionality exists.

## Generating a statechart migration

`statecharts/lightswitch_flow-1.0.scxml` is the chart. Turn it into a sqitch
migration and deploy it:

```bash
$ make gen-charts
docker compose exec db psql -c "$GEN_CHARTS_QUERY"
CREATE EXTENSION
INFO:  created new migration: statechart/lightswitch_flow-1.0.0
 gen_statechart_sqitch_migrations
----------------------------------

(1 row)

$ ls -l sqitch/deploy/statechart/
-rw-r--r-- 1 you you 855 Aug  2 13:19 lightswitch_flow-1.0.0.sql

$ make deploy-migrations
Deploying changes to db:postgres://postgres@localhost:5432/postgres
  + statechart/lightswitch_flow-1.0.0 .. ok
```

Note that the generated file belongs to you. In a real project you would commit
it together with the new `sqitch.plan` line.

Rerunning `make gen-charts` rewrites the files and leaves the plan alone, so it
is safe to regenerate after editing the `.scxml`.

`make import-charts` is the shortcut: it loads the charts straight into the
database without producing migrations. Useful while iterating, but nothing
records that it happened.

## Using the state machine

`make access-database`, then:

```sql
postgres=# insert into lightswitch default values;
INSERT 0 1
postgres=# select id, state_machine_id, turned_on_at, turned_off_at from lightswitch;
 id | state_machine_id | turned_on_at |         turned_off_at
----+------------------+--------------+-------------------------------
  1 |                1 |              | 2026-08-02 11:19:30.885771+00

postgres=# select fsm.notify_state_machine(shard => 1, machine => state_machine_id, event => 'lightswitch.turn_on') from lightswitch;
postgres=# select id, turned_on_at, turned_off_at from lightswitch;
 id |         turned_on_at          | turned_off_at
----+-------------------------------+---------------
  1 | 2026-08-02 11:19:37.543633+00 |
```

## How the two extensions are split

`sqitch/deploy/lightswitch.sql` creates only `pg_statecharts`, the runtime
half. That is what a production database gets.

`pg_statecharts_dev` is created by the `gen-charts` and `import-charts` queries
themselves, with a `create extension if not exists` in front of the call, and
never by a migration. It is the half that reads and writes files on the
database host, and there is no reason for it to exist in production.

## Notes on the container

- `user: "${UID}:${GID}"` in `docker-compose.yml` is what makes generated files
  belong to you. `make up` writes the `.env` that supplies those values. This
  replaced the old `chmod 666` / `file_permission_666` workaround.
- The data directory volume is mounted at `/var/lib/postgresql`, not at the
  data directory itself. The official image ships that path mode 1777 so it can
  be run as an arbitrary uid.
- `PGUSER` and `SQITCH_FULLNAME`/`SQITCH_EMAIL` are set because an arbitrary
  uid has no `/etc/passwd` entry for psql and sqitch to look themselves up in.
  Mounting `~/.sqitch` into the container, the old way of telling sqitch who
  you are, does not work any more for the same reason: there is no home
  directory for it to land in. `make setup` takes the name and email from
  your git config and puts them in `.env`.
- The image installs the extensions by copying files. No compiler, no PGXN
  client, no server headers.
- `mkdir -p sqitch/{deploy,revert,verify}/statechart` is done by `make setup`,
  because SQL cannot create directories.

## Tearing down

```bash
make down                # keeps the data volume
docker compose down -v   # removes it too
```
