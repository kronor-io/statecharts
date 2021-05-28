-- Deploy statecharts:function/get_initial_state to pg

BEGIN;

  create or replace function fsm.get_initial_state(statechart bigint)
  returns setof fsm.state as
  $$
      select s.*
      from fsm.state s
      where s.statechart_id = statechart
        and s.is_initial
        and parent_id is null
  $$ language sql stable strict;

COMMIT;
