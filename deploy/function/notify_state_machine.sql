-- Deploy statecharts:function/notify_state_machine to pg

BEGIN;

    create or replace function fsm.notify_state_machine(shard bigint, machine bigint, event text, data jsonb default '{}')
        returns void as
    $$
        insert into fsm.state_machine_event
            (shard_id, state_machine_id, name, data) values
            (shard, machine, event, data)
    $$ language sql volatile security definer;

    comment on function fsm.notify_state_machine is 'send an event to a specific state machine';

COMMIT;
