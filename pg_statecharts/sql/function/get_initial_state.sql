-- Returns all the chains of nodes that are marked as initial. The chain
-- is interrupted whenever there is a child marked a non-initial
--
-- `statechart` is the name this took before 0.2.0, kept so that callers that
-- name their arguments keep working, and removed in 0.3.0. statechart_id is
-- qualified at every use because fsm.state has a column of that name, and in a
-- SQL function the column would quietly win.
create or replace function fsm.get_initial_state(
    statechart_id bigint default null
  -- deprecated, removed in 0.3.0
  , statechart    bigint default null
)
returns setof fsm.state as
$$
    with root as (
      select s.*
      from fsm.state s
      where s.statechart_id = coalesce(get_initial_state.statechart_id, get_initial_state.statechart)
        and s.is_initial
        and parent_id is null
    )
    select * from root

    union

    select child_s.*
    from fsm.state child_s
    where child_s.statechart_id = coalesce(get_initial_state.statechart_id, get_initial_state.statechart)
      and child_s.is_initial
      and child_s.parent_path <@ (select node_path from root)
      -- we make sure that the chaing of parents only contain initial nodes
      and not exists (
        select *
        from fsm.state parent
        -- <@ = left is descendant or equals to right
        where child_s.node_path <@ parent.node_path
          and not parent.is_initial
      )

$$ language sql stable parallel safe;

comment on function fsm.get_initial_state(bigint, bigint) is $comment$
    Get all initial states, when an initial state has children all of its
    initial states are also returned.
$comment$;
