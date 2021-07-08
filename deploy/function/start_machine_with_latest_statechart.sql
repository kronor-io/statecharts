-- Deploy statecharts:function/start_machine_with_latest_statechart to pg

BEGIN;

    create or replace function fsm.start_machine_with_latest_statechart(shard_id bigint, named text, initial_data jsonb default '{}')
    returns setof fsm.state_machine as
    $$
        with desired_chart as (
            select id from fsm.get_latest_statechart(named)
        )
        select m.*
        from desired_chart
        join fsm.start_machine(shard_id, desired_chart.id, initial_data) m on true
        limit 1
    $$ language sql volatile strict
        rows 1;

COMMIT;
