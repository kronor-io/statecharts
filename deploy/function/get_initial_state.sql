-- Deploy statecharts:function/get_initial_state to pg

BEGIN;

  create or replace function fsm.get_initial_state(statechart bigint)
  returns setof fsm.state as
  $$
      select s.*
      from fsm.state s
      where s.statechart_id = statechart
        and s.is_initial
        --
        -- The global initial state for a statechart can be defined as the
        -- state that never appears as the child of another state.
        --
        and (s.statechart_id, s.id) not in (
            select c.statechart_id, c.child_state
            from fsm.compound_state c
            where c.statechart_id = statechart
        )
  $$ language sql stable strict;

COMMIT;
