#!/bin/bash
# Checks that a schema deployed with sqitch can be adopted into the extension
# and then upgraded to the current version, ending up indistinguishable from a
# fresh install.
#
# Needs sqitch and the semver extension, which the sqitch deployment depends
# on, plus the extension files already installed.
set -eu

cd "$(dirname "$0")/../.."

PSQL="psql -v ON_ERROR_STOP=1 -X -q"
VERSION=$(sed -n "s/^default_version *= *'\([^']*\)'.*/\1/p" pg_statecharts/pg_statecharts.control)

members() {
  # Every object the extension owns, described the same way in any database.
  $PSQL -tA -d "$1" -c "
    select pg_describe_object(classid, objid, objsubid)
    from pg_depend
    where refclassid = 'pg_extension'::regclass
      and refobjid = (select oid from pg_extension where extname = 'pg_statecharts')
      and deptype = 'e'
    order by 1"
}

echo "--- deploying the sqitch schema with data ---"
$PSQL -c "drop database if exists adopt_check"
$PSQL -c "create database adopt_check"
sqitch deploy --quiet db:pg:adopt_check

$PSQL -d adopt_check <<'SQL'
insert into fsm.statechart (name, version) values
  ('flow', to_semver('1.9')), ('flow', to_semver('1.10.0')), ('flow', to_semver('2.0.0'));

insert into fsm.state (statechart_id, is_initial, is_final, id, name, parent_path, node_path)
select id, true, false, 'start', 'S', id::text::ltree, (id::text || '.start')::ltree
from fsm.statechart where version = '2.0.0';
SQL

# The migration the README tells projects to add. It has to do the right
# thing on a database deployed with sqitch, on a fresh one, and when run again,
# so the same text is used against every database below.
MIGRATION=$(cat <<'SQL'
do $migrate$
begin
  if exists (select 1 from pg_namespace where nspname = 'fsm')
     and not exists (select 1 from pg_extension where extname = 'pg_statecharts') then
    create extension pg_statecharts version 'sqitch';
  else
    create extension if not exists pg_statecharts cascade;
  end if;
  alter extension pg_statecharts update;
end
$migrate$;
SQL
)

echo "--- adopting: the 'sqitch' version alone must adopt and change nothing else ---"
$PSQL -d adopt_check -c "create extension pg_statecharts version 'sqitch'"

$PSQL -d adopt_check <<'SQL'
do $check$
declare
  stray text;
begin
  if (select extversion from pg_extension where extname = 'pg_statecharts') <> 'sqitch' then
    raise exception 'extension was not created at the sqitch version';
  end if;

  -- Anything left in the fsm schema that the extension does not own would be
  -- missed by DROP EXTENSION and by pg_dump, so the adoption has to be total.
  select string_agg(pg_describe_object(c.classid, c.objid, 0), ', ')
  into stray
  from (
    select 'pg_class'::regclass as classid, oid as objid from pg_class
      where relnamespace = 'fsm'::regnamespace and relkind in ('r', 'S', 'v', 'm')
    union all
    select 'pg_proc'::regclass, oid from pg_proc where pronamespace = 'fsm'::regnamespace
    union all
    select 'pg_type'::regclass, oid from pg_type
      where typnamespace = 'fsm'::regnamespace and typtype in ('c', 'd', 'e') and typrelid = 0
  ) c
  where not exists (
    select 1 from pg_depend d
    where d.classid = c.classid and d.objid = c.objid and d.deptype = 'e'
      and d.refobjid = (select oid from pg_extension where extname = 'pg_statecharts')
  );

  if stray is not null then
    raise exception 'objects in the fsm schema not adopted: %', stray;
  end if;

  raise notice 'every object in fsm now belongs to the extension';
end
$check$;
SQL

echo "--- upgrading, with the migration a project would ship ---"
# The extension already exists at 'sqitch' here, so this takes the else branch
# and then updates. Running it against the sqitch schema from scratch is
# covered by the fresh database below taking the other branch.
$PSQL -d adopt_check -c "$MIGRATION"

echo "--- verifying ---"
$PSQL -d adopt_check -v version="$VERSION" <<'SQL'
-- psql does not substitute variables inside dollar quotes, so hand the
-- expected version to the DO block through a setting instead.
select set_config('check.version', :'version', false) as configured \gset

do $check$
declare
  latest text;
  kept int;
  not_dumped text;
begin
  if (select extversion from pg_extension where extname = 'pg_statecharts') <> current_setting('check.version') then
    raise exception 'extension was not upgraded to %', current_setting('check.version');
  end if;

  -- The adopted tables and sequences have to end up registered with pg_dump
  -- like a fresh install's, or the backups of a migrated database keep
  -- missing the machines.
  select string_agg(c.oid::regclass::text, ', ' order by c.oid::regclass::text)
  into not_dumped
  from pg_class c
  where c.relnamespace = 'fsm'::regnamespace
    and c.relkind in ('r', 'S')
    and not exists (
      select 1 from pg_extension e
      where e.extname = 'pg_statecharts' and c.oid = any (e.extconfig)
    );
  if not_dumped is not null then
    raise exception 'not registered for pg_dump after adoption: %', not_dumped;
  end if;

  if exists (select 1 from pg_extension where extname = 'semver') then
    raise exception 'the semver extension should have been dropped';
  end if;

  if (select atttypid::regtype::text
      from pg_attribute
      where attrelid = 'fsm.statechart'::regclass and attname = 'version') <> 'fsm.semver' then
    raise exception 'version column was not converted to fsm.semver';
  end if;

  select count(*) into kept from fsm.statechart;
  if kept <> 3 then
    raise exception 'expected 3 statecharts after the upgrade, found %', kept;
  end if;

  select fsm.semver_text(version) into latest from fsm.get_latest_statechart('flow');
  if latest <> '2.0.0' then
    raise exception 'get_latest_statechart returned %, expected 2.0.0', latest;
  end if;

  -- migrations generated by the Rust build call an unqualified to_semver()
  if not exists (select 1 from fsm.statechart where name = 'flow' and version = to_semver('1.10.0')) then
    raise exception 'to_semver() equality no longer matches stored versions';
  end if;

  if not exists (select 1 from fsm.state where id = 'start') then
    raise exception 'existing state rows did not survive';
  end if;

  raise notice 'adoption and upgrade verified';
end
$check$;
SQL

echo "--- the same migration on a fresh database, twice ---"
# A hardcoded "version 'sqitch'" fails on a database without the legacy
# schema, which is what every fresh dev database is. The migration has to
# take the other branch there, and be harmless when run again.
$PSQL -c "drop database if exists adopt_fresh"
$PSQL -c "create database adopt_fresh"
$PSQL -d adopt_fresh -c "$MIGRATION"
$PSQL -d adopt_fresh -c "$MIGRATION"
if [ "$($PSQL -tA -d adopt_fresh -c "select extversion from pg_extension where extname = 'pg_statecharts'")" != "$VERSION" ]; then
  echo "error: the migration did not install $VERSION on a fresh database" >&2
  exit 1
fi

echo "--- and the legacy schema migrated in one go ---"
# Deploy the sqitch schema again and run only the migration, so that the
# 'sqitch' branch is exercised end to end rather than in two steps.
$PSQL -c "drop database if exists adopt_onego"
$PSQL -c "create database adopt_onego"
sqitch deploy --quiet db:pg:adopt_onego
$PSQL -d adopt_onego -c "insert into fsm.statechart (name, version) values ('flow', to_semver('1.2.3'))"
$PSQL -d adopt_onego -c "$MIGRATION"
$PSQL -d adopt_onego -c "$MIGRATION"
if [ "$($PSQL -tA -d adopt_onego -c "select fsm.semver_text(version) from fsm.statechart")" != "1.2.3" ]; then
  echo "error: the one-step migration lost or mangled the data" >&2
  exit 1
fi

echo "--- comparing with a fresh install ---"
for adopted in adopt_check adopt_onego; do
  if ! diff <(members adopt_fresh) <(members "$adopted"); then
    echo "error: $adopted does not own the same objects as a fresh install" >&2
    echo "(< fresh install, > adopted)" >&2
    exit 1
  fi
done
echo "the adopted extensions own exactly what a fresh install does"

echo "--- checking sqitch can no longer drop what the extension owns ---"
# Reverting this far first undoes the two handle_machine_events reworks, whose
# revert scripts are CREATE OR REPLACE and therefore go through, swapping old
# bodies into a function the extension now owns. That is exactly the hazard
# the README warns about. The next change back drops a type, and that is where
# the extension's protection has to kick in.
if sqitch revert --quiet -y --to function/is_state_active db:pg:adopt_check 2>/dev/null; then
  echo "error: sqitch was able to revert past an object the extension owns" >&2
  exit 1
fi
echo "sqitch revert of an extension-owned object is refused"

echo "all adoption checks passed"
