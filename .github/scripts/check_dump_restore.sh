#!/bin/bash
# Checks that pg_dump includes the fsm tables and that the dump restores.
#
# pg_dump leaves out an extension's tables unless the extension registers them
# with pg_extension_config_dump. 0.0.0 did not, so a backup of a database using
# it held every application row with its state_machine_id and none of the
# machines: after a restore the fsm tables were empty, the identity sequences
# started over, and new machines collided with the ids the application still
# carried. This runs a machine, dumps the database, restores the dump into a
# fresh one and compares the two.
#
# The restore is also where pg_restore's empty search_path bites. The triggers
# on fsm.state and fsm.transition fire while the data loads, so their ltree
# references have to carry a schema rather than rely on the search_path.
set -eu

PSQL="psql -v ON_ERROR_STOP=1 -X -q"
DUMP=$(mktemp)
trap 'rm -f "$DUMP"' EXIT

echo "--- building a database with a running machine ---"
$PSQL -c "drop database if exists dump_check"
$PSQL -c "drop database if exists dump_restored"
$PSQL -c "create database dump_check"
$PSQL -d dump_check <<'SQL'
create extension pg_statecharts cascade;

create table lightswitch (
  id bigint generated always as identity primary key,
  state_machine_id bigint not null unique,
  turned_on_at timestamptz,
  turned_off_at timestamptz
);

create function turn_on_lightswitch(p fsm_event_payload) returns void as $$
  update lightswitch set turned_on_at = now(), turned_off_at = null
  where state_machine_id = (p).machine_id
$$ language sql;

create function turn_off_lightswitch(p fsm_event_payload) returns void as $$
  update lightswitch set turned_on_at = null, turned_off_at = now()
  where state_machine_id = (p).machine_id
$$ language sql;

-- 'on' and 'off' sit inside a composite state, so that the parent path trigger
-- has a parent to look up when the rows are loaded back.
do $$
declare chart bigint;
begin
  insert into fsm.statechart (name, version) values ('lightswitch_flow', fsm.to_semver('1.0.0'))
  returning id into chart;

  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry) values
    (chart, 'powered', 'POWERED', null, true, false, array[]::fsm_callback_name[]),
    (chart, 'off', 'OFF', 'powered', true, false, array[('public', 'turn_off_lightswitch')]::fsm_callback_name[]),
    (chart, 'on', 'ON', 'powered', false, false, array[('public', 'turn_on_lightswitch')]::fsm_callback_name[]);

  insert into fsm.transition (statechart_id, event, source_state, target_state) values
    (chart, 'lightswitch.turn_on', 'off', 'on'),
    (chart, 'lightswitch.turn_off', 'on', 'off');
end $$;

insert into lightswitch (state_machine_id)
select id from fsm.create_state_machine_with_latest_statechart(1, 'lightswitch_flow');

select fsm.start_machine(1, state_machine_id) from lightswitch;
select fsm.notify_state_machine(1, state_machine_id, 'lightswitch.turn_on') from lightswitch;
select fsm.notify_state_machine(1, state_machine_id, 'lightswitch.turn_off') from lightswitch;
SQL

# Everything that has to survive the round trip, in a stable order. The
# sequences are read directly: pg_dump restores their last_value with setval,
# so a restored database has to continue numbering where the dumped one
# stopped rather than start again from 1.
snapshot() {
  $PSQL -tA -d "$1" <<'SQL'
select 'statechart', count(*) from fsm.statechart
union all select 'state', count(*) from fsm.state
union all select 'transition', count(*) from fsm.transition
union all select 'state_machine', count(*) from fsm.state_machine
union all select 'state_machine_state', count(*) from fsm.state_machine_state
union all select 'state_machine_event', count(*) from fsm.state_machine_event
order by 1;
select id, parent_id, parent_path::text, node_path::text from fsm.state order by id;
select state_id, exited_at is null as active from fsm.state_machine_state order by entered_at, state_id;
select name, handled_at is not null as handled from fsm.state_machine_event order by created_at, id;
select 'statechart_id_seq', last_value, is_called from fsm.statechart_id_seq
union all select 'state_machine_id_seq', last_value, is_called from fsm.state_machine_id_seq
union all select 'state_machine_event_id_seq', last_value, is_called from fsm.state_machine_event_id_seq;
SQL
}

# Every table and sequence in fsm has to be registered, or a new table added
# later would silently fall out of the backups again.
check_all_registered() {
  $PSQL -d "$1" <<'SQL'
do $check$
declare
  missing text;
begin
  select string_agg(c.oid::regclass::text, ', ' order by c.oid::regclass::text)
  into missing
  from pg_class c
  where c.relnamespace = 'fsm'::regnamespace
    and c.relkind in ('r', 'S')
    and not exists (
      select 1 from pg_extension e
      where e.extname = 'pg_statecharts' and c.oid = any (e.extconfig)
    );
  if missing is not null then
    raise exception 'not registered for pg_dump: %', missing;
  end if;
end
$check$;
SQL
}

check_all_registered dump_check
snapshot dump_check > "$DUMP.before"
echo "$(wc -l < "$DUMP.before") lines of state before the dump"

echo "--- dumping ---"
pg_dump -Fc -f "$DUMP" dump_check

tables=$(pg_restore -l "$DUMP" | grep -c '^[0-9]*; [0-9]* [0-9]* TABLE DATA fsm ' || true)
sequences=$(pg_restore -l "$DUMP" | grep -c '^[0-9]*; [0-9]* [0-9]* SEQUENCE SET fsm ' || true)
if [ "$tables" -ne 6 ] || [ "$sequences" -ne 3 ]; then
  echo "error: expected 6 fsm tables and 3 fsm sequences in the dump, found $tables and $sequences" >&2
  pg_restore -l "$DUMP" | grep ' fsm ' >&2 || true
  exit 1
fi
echo "the dump carries all 6 fsm tables and 3 sequences"

echo "--- restoring ---"
$PSQL -c "create database dump_restored"
pg_restore --exit-on-error -d dump_restored "$DUMP"

echo "--- comparing ---"
if ! diff "$DUMP.before" <(snapshot dump_restored); then
  echo "error: the restored database differs from the dumped one (< dumped, > restored)" >&2
  exit 1
fi
rm -f "$DUMP.before"
check_all_registered dump_restored
echo "the restored database matches"

echo "--- the restored machines keep working and new ones get fresh ids ---"
# lightswitch.state_machine_id is unique, so a sequence that had started over
# would fail this insert the way it failed real applications after a restore
# under 0.0.0.
$PSQL -d dump_restored <<'SQL'
insert into lightswitch (state_machine_id)
select id from fsm.create_state_machine_with_latest_statechart(1, 'lightswitch_flow');

select fsm.start_machine(1, state_machine_id) from lightswitch where turned_off_at is null;
select fsm.notify_state_machine(1, state_machine_id, 'lightswitch.turn_on') from lightswitch;

do $check$
declare
  machines int;
  lit int;
begin
  select count(*) into machines from lightswitch;
  if machines <> 2 then
    raise exception 'expected 2 lightswitches, found %', machines;
  end if;

  select count(*) into lit from lightswitch where turned_on_at is not null;
  if lit <> 2 then
    raise exception 'expected both lightswitches to be on after the event, % are', lit;
  end if;

  if (select count(*) from fsm.state_machine_state where state_id = 'on' and exited_at is null) <> 2 then
    raise exception 'expected both machines to be in the on state';
  end if;

  raise notice 'both the restored and the new machine run';
end
$check$;
SQL

$PSQL -c "drop database dump_check"
$PSQL -c "drop database dump_restored"
echo "all dump and restore checks passed"
