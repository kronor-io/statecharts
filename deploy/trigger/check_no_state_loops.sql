-- Deploy statecharts:trigger/check_no_state_loops to pg

BEGIN;

  create or replace function fsm.trig_check_no_state_loops()
  returns trigger as
  $$
  declare the_cycle text[];
  begin

    with recursive chart as (
      select distinct statechart_id
      from changed
    )
    , prev as (
      select cs.statechart_id, cs.id, array[id] as seen, false as cycle
      from fsm.state cs
      join chart
        on  chart.statechart_id = cs.statechart_id
      where parent_id is null

      union all

      select cs.statechart_id, cs.id, seen || cs.id as seen,
          cs.id = any(seen) as cycle
      from prev
      join fsm.state cs
        on  cs.statechart_id = prev.statechart_id
        and cs.parent_id = prev.id
        and prev.cycle = false
      join chart
        on  chart.statechart_id = cs.statechart_id
    )
    select seen into the_cycle
    from prev
    where prev.cycle
    limit 1;

    if the_cycle is not null
    then
      raise exception 'compound states should form a directed graph. Cycle detected: %', the_cycle;
    end if;

    return null;
  end;
  $$ language plpgsql;

  set client_min_messages TO warning;
  drop trigger if exists check_no_state_loops_insert on fsm.state;
  drop trigger if exists check_no_state_loops_update on fsm.state;
  reset client_min_messages;

  create trigger check_no_state_loops_insert
  after insert
  on fsm.state
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_state_loops();

  create trigger check_no_state_loops_update
  after update
  on fsm.state
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_state_loops();

COMMIT;
