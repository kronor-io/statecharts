-- See fsm.create_machine for what the trailing deprecated parameters are for.
create or replace function fsm.create_state_machine_with_latest_statechart(
    statechart_name text   default null
  , shard_id        bigint default 1
  -- deprecated, removed in 0.3.0
  , shard_id_       bigint default null
  , named           text   default null
)
returns fsm.state_machine as
$$
    with desired_chart as (
        select c.id
        from fsm.get_latest_statechart(statechart_name => coalesce(statechart_name, named)) c
    )
    insert into fsm.state_machine (shard_id, statechart_id)
    select
        coalesce(shard_id_, create_state_machine_with_latest_statechart.shard_id) as shard_id,
        desired_chart.id as statechart_id
    from desired_chart
    -- stands in for the `strict` this function carried before the deprecated
    -- names forced defaults on. A null name yields no chart and so no row.
    where coalesce(shard_id_, create_state_machine_with_latest_statechart.shard_id) is not null
    returning *

$$ language sql volatile;

comment on function fsm.create_state_machine_with_latest_statechart(text, bigint, bigint, text) is $comment$
    Creates a new state machine but does not start it.
    You need to call fsm.start_machine after this
$comment$;
