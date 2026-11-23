-- Smoke test that a machine defined through the extension actually runs.

-- Error CONTEXT carries plpgsql line numbers, which would make this
-- expected output break on every unrelated edit.
\set SHOW_CONTEXT never
-- Quiet, so that the expected output is the same whether or not an earlier
-- test in the same database already created the extension.
set client_min_messages to warning;
create extension if not exists pg_statecharts cascade;
reset client_min_messages;

create table lightswitch (
  id bigint not null generated always as identity primary key,
  state_machine_id bigint not null,
  turned_on_at timestamptz,
  turned_off_at timestamptz
);

create function turn_on_lightswitch(event_payload fsm_event_payload) returns void as
$$
  update lightswitch set turned_on_at = now(), turned_off_at = null
  where state_machine_id = (event_payload).machine_id
$$ language sql;

create function turn_off_lightswitch(event_payload fsm_event_payload) returns void as
$$
  update lightswitch set turned_on_at = null, turned_off_at = now()
  where state_machine_id = (event_payload).machine_id
$$ language sql;

do $$
declare
  chart bigint;
begin
  insert into fsm.statechart (name, version) values ('lightswitch_flow', fsm.to_semver('1.0.0'))
  returning id into chart;

  insert into fsm.state (statechart_id, id, name, parent_id, is_initial, is_final, on_entry, on_exit) values
  (chart, 'off', 'OFF', null, true, false, array[('public', 'turn_off_lightswitch')]::fsm_callback_name[], array[]::fsm_callback_name[]),
  (chart, 'on', 'ON', null, false, false, array[('public', 'turn_on_lightswitch')]::fsm_callback_name[], array[]::fsm_callback_name[]);

  insert into fsm.transition (statechart_id, event, source_state, target_state) values
  (chart, 'lightswitch.turn_on', 'off', 'on'),
  (chart, 'lightswitch.turn_off', 'on', 'off');
end $$;

-- State ids may contain capital letters. Guarding this because 0.0.0 shipped
-- '^[a-za-z0-9_]+$' for the constraint, with the A-Z range flattened to a
-- second a-z, and silently rejected them.
insert into fsm.state (statechart_id, id, name, is_initial, is_final)
select id, 'HasCapitals', 'Capitals', false, false from fsm.statechart;
select id from fsm.state where id = 'HasCapitals';

-- ...but the characters it was always meant to reject are still rejected
do $$
begin
  insert into fsm.state (statechart_id, id, name, is_initial, is_final)
  select id, 'not-allowed', 'Hyphen', false, false from fsm.statechart;
  raise exception 'a hyphen should not be allowed in a state id';
exception when check_violation then
  raise notice 'state ids still reject punctuation';
end $$;

delete from fsm.state where id = 'HasCapitals';

-- parent_path / node_path are filled in by the trigger
select id, parent_id, parent_path::text ~ '^[0-9]+$' as parent_path_is_chart_id, node_path::text ~ '^[0-9]+\.' as node_path_prefixed
from fsm.state order by id;

select id as initial_state from fsm.get_initial_state(
  statechart_id => (select id from fsm.get_latest_statechart(statechart_name => 'lightswitch_flow')));

-- start a machine and drive it. shard_id defaults to 1, so it is left out:
-- that default is the whole point of the 0.2.0 argument order.
insert into lightswitch (state_machine_id)
select s.id from fsm.create_state_machine_with_latest_statechart(statechart_name => 'lightswitch_flow') s;

-- the returned row carries a timestamp, so only assert that it started
select fsm.start_machine(machine_id => state_machine_id) is not null as started from lightswitch;

select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;

select fsm.notify_state_machine(machine_id => state_machine_id, event_name => 'lightswitch.turn_on') from lightswitch;
select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;

select fsm.notify_state_machine(machine_id => state_machine_id, event_name => 'lightswitch.turn_off') from lightswitch;
select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;

-- the currently active state is the only one without an exited_at
select state_id from fsm.state_machine_state where exited_at is null;

-- is_state_active reads that same log, so a state the machine has entered and
-- left again is not active. The machine went off -> on -> off, so 'on' is in
-- the log twice over with an exited_at on both rows.
select fsm.is_state_active(machine_id => state_machine_id, state_id => 'off') as off_is_active,
       fsm.is_state_active(machine_id => state_machine_id, state_id => 'on') as on_is_active,
       fsm.is_state_active(machine_id => state_machine_id, state_id => 'never_entered') as unknown_is_active
from lightswitch;

-- the machine is in 'off', so turn_on is valid and turn_off is not
select fsm.is_valid_transition(machine_id => state_machine_id, event_name => 'lightswitch.turn_on') as turn_on_valid,
       fsm.is_valid_transition(machine_id => state_machine_id, event_name => 'lightswitch.turn_off') as turn_off_valid
from lightswitch;

-- Nothing marks events handled after the fact any more: the statement that
-- inserts an event hands it straight to handle_machine_events, and handled_at
-- is stamped by its column default. So nothing is ever left unhandled behind
-- the trigger.
select count(*) filter (where handled_at is null) as still_unhandled
from fsm.state_machine_event;

-- One statement inserting several events fires the trigger once, with all of
-- them in its transition table, so they are handled together in insertion
-- order. The machine is in 'off', and these take it off -> on -> off.
insert into fsm.state_machine_event (shard_id, state_machine_id, name, data)
select 1, state_machine_id, e.name, '{}'::jsonb
from lightswitch, (values ('lightswitch.turn_on'), ('lightswitch.turn_off')) as e(name);

select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;
select state_id as active_after_batch from fsm.state_machine_state where exited_at is null;

-- The three argument form handles exactly the events it is handed, with no
-- lookup at all: this one is never inserted into the queue. It is new in
-- 0.2.0, so it has only ever had these argument names.
select fsm.handle_machine_events(
    machine_id => state_machine_id
  , events     => array[(1, 0, state_machine_id, now(), 'lightswitch.turn_on', '{}')::fsm.machine_event]
) from lightswitch;
select turned_on_at is not null as is_on_after_direct_call from lightswitch;

-- With the trigger disabled nothing hands the events over, so they go in
-- unhandled -- handled_at has to be given explicitly, against the default --
-- and the two argument form picks them up.
alter table fsm.state_machine_event disable trigger notify_machine_event;
insert into fsm.state_machine_event (shard_id, state_machine_id, handled_at, name, data)
select 1, state_machine_id, null, 'lightswitch.turn_off', '{}' from lightswitch;
select turned_off_at is not null as is_off_before_handling from lightswitch;
select fsm.handle_machine_events(machine_id => state_machine_id) from lightswitch;
select turned_off_at is not null as is_off_after_handling from lightswitch;
alter table fsm.state_machine_event enable trigger notify_machine_event;

-- Draining the queue of a machine that does not exist is indistinguishable
-- from draining a machine with nothing pending, so the queue form raises too,
-- and reports it exactly as fsm.start_machine does. One mistake, one message.
do $$
declare
  from_start text;
  from_drain text;
begin
  begin
    perform fsm.start_machine(machine_id => 999999);
  exception when raise_exception then from_start := sqlerrm;
  end;

  begin
    perform fsm.handle_machine_events(machine_id => 999999);
  exception when raise_exception then from_drain := sqlerrm;
  end;

  if from_start is null or from_drain is null then
    raise exception 'both forms should have raised, got % and %', from_start, from_drain;
  end if;
  if from_start <> from_drain then
    raise exception 'the two forms report it differently: % vs %', from_start, from_drain;
  end if;
  raise notice 'both forms report it as: %', from_start;
end $$;

-- The names every argument carried before 0.2.0 still resolve, which is what
-- lets a database be upgraded before the application that calls it is
-- redeployed. Each of these is the pre-0.2.0 spelling of a call above.
insert into lightswitch (state_machine_id)
select s.id from fsm.create_state_machine_with_latest_statechart(shard_id_ => 1, named => 'lightswitch_flow') s;

create temporary view newest as
  select state_machine_id from lightswitch order by id desc limit 1;

select fsm.start_machine(shard => 1, machine_id => state_machine_id) is not null as started
from newest;

select fsm.notify_state_machine(shard => 1, machine => state_machine_id, event => 'lightswitch.turn_on')
from newest;

select fsm.is_state_active(shid => 1, smid => state_machine_id, state => 'on') as on_is_active,
       fsm.is_valid_transition(shard => 1, machine_id => state_machine_id, event_ => 'lightswitch.turn_off') as turn_off_valid
from newest;

-- the two argument handle_machine_events under its old spelling; nothing is
-- pending, so it finds no events and does nothing
select fsm.handle_machine_events(shard => 1, machine_id => state_machine_id)
from newest;

select fsm.create_machine(
         shard => 1,
         statechart => (select id from fsm.get_latest_statechart(named => 'lightswitch_flow'))
       ) is not null as created,
       (select count(*) from fsm.get_initial_state(
          statechart => (select id from fsm.get_latest_statechart(named => 'lightswitch_flow')))) as initial_states;

-- The shard argument still reaches the row under either spelling. Getting the
-- coalesce the wrong way round here would silently put every machine in the
-- default shard, so both are asserted.
select (fsm.create_machine(
          statechart_id => (select id from fsm.get_latest_statechart(statechart_name => 'lightswitch_flow')),
          shard_id => 2)).shard_id as new_name_shard,
       (fsm.create_machine(
          statechart => (select id from fsm.get_latest_statechart(named => 'lightswitch_flow')),
          shard => 3)).shard_id as old_name_shard;

-- start_machine_with_latest_statechart, under both spellings
select count(*) as started_new_name
from fsm.start_machine_with_latest_statechart(statechart_name => 'lightswitch_flow');
select count(*) as started_old_name
from fsm.start_machine_with_latest_statechart(shard_id => 1, named => 'lightswitch_flow');

-- a name that matches no chart yields no rows, as it did when the function
-- was strict
select count(*) as no_such_chart
from fsm.start_machine_with_latest_statechart(statechart_name => 'no_such_flow');

-- The opposite case. Above, a null id legitimately means "no chart matched".
-- Here a real id names no machine, which is always a mistake: there is
-- nothing to start, and returning null used to hide it until some unrelated
-- call failed much later.
do $$
declare
  failed boolean := false;
begin
  begin
    perform fsm.start_machine(machine_id => 999999);
  exception when raise_exception then
    failed := true;
  end;
  if not failed then
    raise exception 'starting a machine that does not exist should have raised';
  end if;
  raise notice 'starting an unknown machine raises';
end $$;

-- The usual way to meet that: created under one shard, started under another.
do $$
declare
  elsewhere bigint;
  failed boolean := false;
begin
  select id into elsewhere
  from fsm.create_state_machine_with_latest_statechart(
      statechart_name => 'lightswitch_flow'
    , shard_id        => 7);

  begin
    perform fsm.start_machine(machine_id => elsewhere);
  exception when raise_exception then
    failed := true;
  end;
  if not failed then
    raise exception 'starting a machine from the wrong shard should have raised';
  end if;

  -- and under the shard it was created in it still starts
  perform fsm.start_machine(machine_id => elsewhere, shard_id => 7);
  raise notice 'the wrong shard raises, the right one starts';
end $$;

drop view newest;

-- pg_dump only includes an extension's tables when they are registered as
-- configuration tables. Every table and sequence in fsm has to be, or a backup
-- silently loses the machines while keeping the application rows that point at
-- them. The one exception is the event queue, which is drained in the
-- transaction that fills it and is kept out of the dump on purpose, so expect
-- exactly it and its sequence.
select c.relkind, c.oid::regclass as not_registered_for_pg_dump
from pg_class c
where c.relnamespace = 'fsm'::regnamespace
  and c.relkind in ('r', 'S')
  and not exists (
    select 1 from pg_extension e
    where e.extname = 'pg_statecharts' and c.oid = any (e.extconfig)
  )
order by 1, 2;

drop table lightswitch cascade;
delete from fsm.statechart;
