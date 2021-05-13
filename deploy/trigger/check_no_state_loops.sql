-- Deploy statecharts:trigger/check_no_state_loops to pg

BEGIN;

  create or replace function fsm.trig_check_no_state_loops()
  returns trigger as
  $$
  declare multiple_parents text;
  declare the_cycle text[];
  begin

    select child_state into multiple_parents
    from fsm.compound_state
    where statechart_id = coalesce(NEW.statechart_id, OLD.statechart_id)
    group by child_state
    having count(*) > 1;

    raise notice 'what %', multiple_parents;

    if multiple_parents is not null
    then
      raise exception 'compound states should form a directed graph. State with two parents: %', multiple_parents;
    end if;

    with recursive prev as (
      select cs.parent_state, array[parent_state] as seen, false as cycle
      from fsm.compound_state cs
      where cs.statechart_id = coalesce(NEW.statechart_id, OLD.statechart_id)

      union all

      select cs.parent_state, seen || cs.parent_state as seen,
          cs.parent_state = any(seen) as cycle
      from prev
      inner join fsm.compound_state cs
        on prev.parent_state = cs.child_state
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
  drop trigger if exists check_no_state_loops on fsm.compound_state;
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
