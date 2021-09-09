-- Deploy statecharts:function/get_initial_state to pg

BEGIN;

  -- Returns all the chains of nodes that are marked as initial. The chain
  -- is interrupted whenever there is a child marked a non-initial
  create or replace function fsm.get_initial_state(statechart bigint)
  returns setof fsm.state as
  $$
      with root as (
        select s.*
        from fsm.state s
        where s.statechart_id = statechart
          and s.is_initial
          and parent_id is null
      )
      select * from root

      union

      select child_s.*
      from fsm.state child_s
      where child_s.statechart_id = statechart
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

  $$ language sql stable strict parallel safe;

  comment on function fsm.get_initial_state(bigint) is $comment$
      Get all initial states, when an initial state has children all of its
      initial states are also returned.
  $comment$;

COMMIT;
