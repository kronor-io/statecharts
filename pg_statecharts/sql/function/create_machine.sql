-- shard_id is last and defaulted: most databases are not sharded, so the
-- common call is fsm.create_machine(statechart_id => ...).
--
-- The trailing parameters are the names this function used before 0.2.0. They
-- are kept so that callers that name their arguments keep working across the
-- upgrade, and they are removed in 0.3.0. coalesce reads the old name first
-- because the new one carries a default that would otherwise always win.
create or replace function fsm.create_machine(
    statechart_id bigint default null
  , shard_id      bigint default 1
  -- deprecated, removed in 0.3.0
  , shard         bigint default null
  , statechart    bigint default null
)
  returns fsm.state_machine as
  $$
      -- The deprecated names have to default to null to be omittable, which
      -- rules out `strict`. The where clause takes its place: a null id yields
      -- no row, and the function returns null, as it did when it was strict.
      insert into fsm.state_machine (shard_id, statechart_id)
      select coalesce(shard, create_machine.shard_id)
           , coalesce(create_machine.statechart_id, statechart)
      where coalesce(shard, create_machine.shard_id) is not null
        and coalesce(create_machine.statechart_id, statechart) is not null
      returning *;
  $$ language sql volatile;

comment on function fsm.create_machine(bigint, bigint, bigint, bigint) is $comment$
    creates a state machine without starting it
$comment$;
