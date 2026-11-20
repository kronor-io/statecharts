-- An event as fsm.handle_machine_events receives it: the columns of
-- fsm.state_machine_event that handling reads.
--
-- A type of its own rather than the table's row type on purpose. A function
-- whose signature names fsm.state_machine_event%rowtype depends on that table,
-- and PostgreSQL then refuses to drop or replace the table while the function
-- exists. Deployments that swap the event table out from under the extension
-- (truncating a large queue by swapping in a fresh table, say) need that to
-- stay possible.
create type fsm.machine_event as
( shard_id bigint
, id bigint
, state_machine_id bigint
, created_at timestamptz
, name text
, data jsonb
);

comment on type fsm.machine_event is
    'An event as fsm.handle_machine_events receives it.';
