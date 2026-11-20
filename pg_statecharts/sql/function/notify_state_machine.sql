-- See fsm.create_machine for what the trailing deprecated parameters are for.
-- This function was never strict, so a null id still reaches the insert and is
-- rejected by the column's not null constraint, as it was before 0.2.0.
create or replace function fsm.notify_state_machine(
    machine_id bigint default null
  , event_name text   default null
  , data       jsonb  default '{}'
  , shard_id   bigint default 1
  -- deprecated, removed in 0.3.0
  , shard      bigint default null
  , machine    bigint default null
  , event      text   default null
)
    returns void as
$$
    insert into fsm.state_machine_event
        (shard_id, state_machine_id, name, data) values
        ( coalesce(shard, notify_state_machine.shard_id)
        , coalesce(notify_state_machine.machine_id, machine)
        , coalesce(event_name, event)
        , notify_state_machine.data
        )
$$ language sql volatile security definer;

comment on function fsm.notify_state_machine is 'send an event to a specific state machine';
