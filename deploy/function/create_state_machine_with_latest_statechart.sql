-- Deploy statecharts:function/create_state_machine_with_latest_statechart to pg

BEGIN;

    create or replace function fsm.create_state_machine_with_latest_statechart(shard_id_ bigint, named text)
    returns fsm.state_machine as
    $$
        with desired_chart as (
            select id from fsm.get_latest_statechart(named)
        )
        insert into fsm.state_machine (shard_id, statechart_id)
        select
            shard_id_ as shard_id,
            desired_chart.id as statechart_id
        from desired_chart
        returning *

    $$ language sql volatile strict;

    comment on function fsm.create_state_machine_with_latest_statechart(bigint, text) is $comment$
        Creates a new state machine but does not start it.
        You need to call fsm.start_machine after this
    $comment$;

COMMIT;
