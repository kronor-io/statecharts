-- Deploy statecharts:function/get_initial_state to pg

BEGIN;

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
  $$ language sql stable strict parallel safe;

COMMIT;
