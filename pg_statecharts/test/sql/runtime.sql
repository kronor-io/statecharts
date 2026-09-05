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

select id as initial_state from fsm.get_initial_state((select id from fsm.get_latest_statechart('lightswitch_flow')));

-- start a machine and drive it
insert into lightswitch (state_machine_id)
select s.id from fsm.create_state_machine_with_latest_statechart(1, 'lightswitch_flow') s;

-- the returned row carries a timestamp, so only assert that it started
select fsm.start_machine(1, state_machine_id) is not null as started from lightswitch;

select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;

select fsm.notify_state_machine(1, state_machine_id, 'lightswitch.turn_on') from lightswitch;
select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;

select fsm.notify_state_machine(1, state_machine_id, 'lightswitch.turn_off') from lightswitch;
select turned_on_at is not null as is_on, turned_off_at is not null as is_off from lightswitch;

-- the currently active state is the only one without an exited_at
select state_id from fsm.state_machine_state where exited_at is null;

-- the machine is in 'off', so turn_on is valid and turn_off is not
select fsm.is_valid_transition(1, state_machine_id, 'lightswitch.turn_on') as turn_on_valid,
       fsm.is_valid_transition(1, state_machine_id, 'lightswitch.turn_off') as turn_off_valid
from lightswitch;

-- pg_dump only includes an extension's tables when they are registered as
-- configuration tables. Every table and sequence in fsm has to be, or a backup
-- silently loses the machines while keeping the application rows that point at
-- them. Expect no rows.
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
