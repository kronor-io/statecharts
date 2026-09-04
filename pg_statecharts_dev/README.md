# pg_statecharts_dev

Development tooling for [pg_statecharts](../pg_statecharts): turns `.scxml`
files into statecharts in the database, either directly or as sqitch
migrations.

This is deliberately a separate extension. Both of its functions read files
from the database host, and one of them writes files too. That is fine on a
development machine and has no place on a production server, so production
installs only get `pg_statecharts` and never create this one.

## Installing

```bash
./install.sh
```

```sql
create extension pg_statecharts_dev cascade;
```

`cascade` pulls in `pg_statecharts`, which is the only dependency. Like the
runtime half, this is pure SQL: nothing to compile, same files everywhere.

The prepackaged versions on the
[releases page](https://github.com/kronor-io/statecharts/releases) keep the
two apart as well. The Debian package for this extension depends on the runtime
package of the exact same version, so `dpkg` refuses to install it alone:

```bash
sudo dpkg -i pg-statecharts-18_0.1.0.deb pg-statecharts-dev-18_0.1.0.deb
```

The release tarball contains both extensions, and its top-level `install.sh`
only installs this one when passed `--dev`:

```bash
./install.sh --dev
```

## Permissions

Both functions run inside the PostgreSQL server process, so **paths are
resolved on the database host, not on your machine**. When PostgreSQL runs in a
container, pass the paths as they look inside the container.

The calling role needs the file access roles, which superuser has already:

```sql
grant pg_read_server_files  to my_dev_user;   -- import_scxml_files
grant pg_write_server_files to my_dev_user;   -- gen_statechart_sqitch_migrations
```

### Generated files and Docker

Because the server writes the files, they are owned by the user the server runs
as. With a stock Postgres container that is uid 999, and the migrations land in
your repository owned by someone you are not.

The fix is to run the container as yourself:

```yaml
services:
  db:
    image: postgres:18
    user: "${UID:-1000}:${GID:-1000}"
    environment:
      PGUSER: postgres          # an arbitrary uid has no /etc/passwd entry
    volumes:
      # mount at the parent, not at the data directory: the official image
      # ships /var/lib/postgresql mode 1777 so it can run as any uid
      - dbdata:/var/lib/postgresql
      - .:/repo
```

`docker compose` will not expand `$(id -u)` itself, so put the values in a
`.env` file next to the compose file:

```bash
printf 'UID=%s\nGID=%s\n' "$(id -u)" "$(id -g)" > .env
```

See [../example](../example) for a working setup. This replaces the old
`file_permission_666` flag, which no longer exists.

## fsm.import_scxml_files

```sql
select * from fsm.import_scxml_files(
  source_path            => '/repo/statecharts',
  recursive              => false,
  on_conflict_do_nothing => false
);
```

Reads every `.scxml` file at `source_path` and inserts it into the statechart
tables, returning the `fsm.statechart` rows it created. `source_path` may be a
single file or a directory.

Everything happens in your transaction, so a file that fails to parse half way
through a directory leaves nothing behind. Every `on_entry` and `on_exit`
callback is checked to exist and to take one `fsm_event_payload` argument
before the import is allowed to succeed.

This is the quick loop for iterating on a chart. Nothing records that it
happened, so use the migration generator for anything you want to keep.

## fsm.gen_statechart_sqitch_migrations

```sql
select fsm.gen_statechart_sqitch_migrations(
  source_path           => '/repo/statecharts',
  sqitch_plan_file_path => '/repo/sqitch/sqitch.plan',
  recursive             => false
);
```

Writes a deploy, revert and verify script for every chart and adds it to the
sqitch plan. A chart that is already in the plan has its files rewritten and
the plan left alone, so regenerating is safe.

The output is byte for byte what the previous Rust implementation produced.
Regenerating an existing chart shows up as no diff at all.

**The output directories have to exist.** SQL cannot create directories, so
this is the one bit of manual setup:

```bash
mkdir -p sqitch/{deploy,revert,verify}/statechart
```

If they are missing you get an error naming all of them at once, and nothing is
written.

## Layout

`sql/` holds one file per group of functions and `sql/parts.txt` lists them in
creation order; `make` and `install.sh` both read that list. Functions prefixed
with `__` are internal.

The parsing entry points, `fsm.scxml_states(xml)` and
`fsm.scxml_transitions(xml)`, take XML directly rather than a path, which makes
them easy to poke at:

```sql
select * from fsm.scxml_states(pg_read_file('/repo/statecharts/x.scxml')::xml);
```

## Tests

```bash
make install
make installcheck
```
