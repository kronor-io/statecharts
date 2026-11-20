-- See fsm.create_machine for what the trailing deprecated parameters are for.
-- shard_id and parent_path are qualified at every use because the tables this
-- reads have columns of both names, and in a SQL function the column wins.
create or replace function fsm.get_finalized_parents(
    machine_id  bigint default null
  , parent_path ltree  default null
  , shard_id    bigint default 1
  -- deprecated, removed in 0.3.0
  , shard                    bigint default null
  , active_state_parent_path ltree  default null
)
  returns setof fsm.state as
$$
  with all_parents as (
   select a_parent.*
   from fsm.state a_parent
   -- check that active_state_path is a child of a_parent
   where a_parent.node_path @> coalesce(get_finalized_parents.parent_path, active_state_parent_path)
     and exists (
        select 1
        from fsm.state_machine_state current_state
        where current_state.shard_id = coalesce(shard, get_finalized_parents.shard_id)
          and current_state.state_machine_id = machine_id
          and current_state.exited_at is null -- the state is currently active
          and current_state.statechart_id = a_parent.statechart_id
     )
  )
  select *
  from all_parents parent
  where
  -- A state can only be finalizable if it has no outgoing transitions. Here, we look
  -- at all the active children for the parent we are inspecting and make sure that
  -- all of them are at a point where the machine cannot move further inside that parent.
  not exists (
    select 1
    from fsm.state s
    join fsm.state_machine_state current_state
      on  current_state.shard_id = coalesce(shard, get_finalized_parents.shard_id)
      and current_state.state_machine_id = machine_id
      and current_state.state_id = s.id
      and current_state.exited_at is null -- the state is currently active
    where parent.id = s.parent_id
      and s.statechart_id = parent.statechart_id
      and exists (
        select 1 -- check that there is at least one outgoing transition for this state
        from fsm.transition t
        where t.statechart_id = s.statechart_id
          and t.source_state = s.id
      )
  )

  -- Parallel states are a special case. A parallel node is finalized when all its internal states
  -- have finilized already. Without this filter, we would end up considering parents with pending
  -- internal transitions.
  -- The key is to "dive-in" into the children of the parent we are inspecting and verify
  -- that only active states are marked as "is_final".
  -- The filter below is expressed in the negative: active non-final states should not exist.
  and not exists (
    select 1
    from fsm.state s -- the possible non-final, active state
    join fsm.state_machine_state current_state -- active parent state
      on  current_state.shard_id = coalesce(shard, get_finalized_parents.shard_id)
      and current_state.state_machine_id = machine_id
      and current_state.state_id = s.id
      and current_state.exited_at is null
    -- Dive into the children of the parent we are inspecting
    where parent.node_path @> s.parent_path -- check that the active parent state is actually a parent
      and parent.statechart_id = s.statechart_id
      -- get only the non "is_final" active states, since they determine wether or not there is
      -- more work to do for this sub-machine
      and not s.is_final -- check that the state under inspection is non-final
      -- Only non-final states can have children. Having an active parent state is fine, so
      -- we only need to make sure that this non-final state is a leaf in the tree. That is,
      -- it should not have any children.
      and not exists (
        select 1
        from fsm.state child
        where s.node_path @> child.parent_path
      )
  )
-- `strict` is gone because the deprecated names have to default to null to be
-- omittable. Nothing replaces it: a null argument makes every comparison below
-- null, so the function still returns no rows.
$$ language sql stable parallel safe;

comment on function fsm.get_finalized_parents (bigint, ltree, bigint, bigint, ltree) is
$comment$
A finalized parent can be defined as a compound state where all its children have reached
a state where no other transitions are possible.

We can determine this property by analyzing the possible transitions for internal active states
and whether or not these active states are final.

This function is used to generate "done.state.*" events for a state machine for all the parents
in an inheritance chain.
$comment$;
