-- Deploy statecharts:function/start_machine_with_latest_statechart to pg

BEGIN;

    create or replace function fsm.start_machine_with_latest_statechart(shard_id bigint, named text, initial_data jsonb default '{}')
    returns setof fsm.state_machine as
    $$
        select m.*
        from fsm.create_state_machine_with_latest_statechart(shard_id, named) m
        join fsm.start_machine(shard_id, m.id, initial_data) on true

        limit 1
    $$ language sql volatile strict
        rows 1;

    comment on function fsm.start_machine_with_latest_statechart(bigint, text, jsonb) is $comment$
        Creates and starts a state machine with the latest statechart with the name passed.
    $comment$;

COMMIT;
