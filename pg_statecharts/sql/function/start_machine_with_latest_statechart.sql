-- shard_id already had this name before 0.2.0, so it needs no deprecated
-- alias; `named` does. shard_id is qualified at every use because the rows
-- this selects from have a column of that name.
create or replace function fsm.start_machine_with_latest_statechart(
    statechart_name text   default null
  , initial_data    jsonb  default '{}'
  , shard_id        bigint default 1
  -- deprecated, removed in 0.3.0
  , named           text   default null
)
returns setof fsm.state_machine as
$$
    select m.*
    from fsm.create_state_machine_with_latest_statechart(
             statechart_name => coalesce(start_machine_with_latest_statechart.statechart_name, named)
           , shard_id        => start_machine_with_latest_statechart.shard_id
         ) m
    join fsm.start_machine(
             machine_id   => m.id
           , initial_data => start_machine_with_latest_statechart.initial_data
           , shard_id     => start_machine_with_latest_statechart.shard_id
         ) on true

    -- create_state_machine_with_latest_statechart returns a null row for a
    -- name that matches no chart, where this function used to return no rows
    -- at all because it was strict
    where m.id is not null

    limit 1
$$ language sql volatile
    rows 1;

comment on function fsm.start_machine_with_latest_statechart(text, jsonb, bigint, text) is $comment$
    Creates and starts a state machine with the latest statechart with the name passed.
$comment$;
