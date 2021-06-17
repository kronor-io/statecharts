-- Deploy statecharts:trigger/check_no_duplicate_event_handler to pg

BEGIN;

  create or replace function fsm.trig_check_no_duplicate_event_handler() returns trigger as
  $$
    declare
        total_occurrences int;
    begin
      -- look at the entire tree to see if the same event name is used
      -- for a transition
      select count(*) into total_occurrences
      from fsm.transition t
      -- need to get the source state info for the transition
      -- specifically, we need to get its node_path
      join fsm.state s
        on  s.statechart_id = t.statechart_id
        and s.id = t.source_state
      -- get all children and parents on the source_state by using the node_path
      -- it is the count of this children nodes that will determine
      -- if the same event is used in another transition
      join fsm.state relative
        on  relative.statechart_id = t.statechart_id
        and (relative.node_path <@ s.node_path or relative.node_path @> s.node_path)
        and relative.id <> s.id
      join fsm.transition ct
        on  ct.statechart_id = relative.statechart_id
        and ct.event = coalesce(NEW.event, OLD.event)
        and ct.source_state = relative.id
      where t.statechart_id = coalesce(NEW.statechart_id, OLD.statechart_id)
        and t.event = coalesce(NEW.event, OLD.event);

      if total_occurrences > 0
      then
        raise exception $m$
        "%" cannot be used as a transition here.
        It is already used for a child or parent state transition
        $m$, coalesce(NEW.event, OLD.event);
      end if;

      return null;
    end;
  $$ language plpgsql;

  set client_min_messages TO warning;
  drop trigger if exists check_no_duplicate_event_handler on fsm.transition;
  reset client_min_messages;

  create constraint trigger check_no_duplicate_event_handler
  after insert or update
  on fsm.transition
  for each row
  execute function fsm.trig_check_no_duplicate_event_handler();

COMMIT;
