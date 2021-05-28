-- Deploy statecharts:trigger/check_no_state_loops to pg

BEGIN;

  create or replace function fsm.trig_check_no_state_loops()
  returns trigger as
  $$
  declare the_cycle text[];
  begin

    with recursive prev as (
      select cs.id, array[id] as seen, false as cycle
      from fsm.state cs
      where cs.statechart_id = coalesce(NEW.statechart_id, OLD.statechart_id)
        and parent_id is null

      union all

      select cs.id, seen || cs.id as seen,
          cs.id = any(seen) as cycle
      from prev
      inner join fsm.state cs
        on prev.id = cs.parent_id
        and cs.statechart_id = coalesce(NEW.statechart_id, OLD.statechart_id)
      and prev.cycle = false
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
  drop trigger if exists check_no_state_loops on fsm.state;
  reset client_min_messages;


  -- By making the triggers INITIALLY DEFERRED, we tell PostgreSQL
  -- to check the condition at COMMIT time. This means that inside a
  -- single transaction we are allowed to be inconsistent, as long as
  -- the final result remains consistent.

  create constraint trigger check_no_state_loops
  after insert or update
  on fsm.state
  deferrable initially deferred
  for each row
  execute function fsm.trig_check_no_state_loops();

COMMIT;
