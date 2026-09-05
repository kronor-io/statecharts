create or replace function fsm.trig_check_no_duplicate_event_handler() returns trigger as
$$
  declare
      duplicate_transitions jsonb;
  begin
    with changed_event as (
      select distinct statechart_id, event
      from changed
    )
    -- look at the entire tree to see if the same event name is used
    -- for a transition
    select jsonb_agg(t.*) into duplicate_transitions
    from fsm.transition t
    -- need to get the source state info for the transition
    -- specifically, we need to get its node_path
    join fsm.state s
      on  s.statechart_id = t.statechart_id
      and s.id = t.source_state
    -- get all children and parents on the source_state by using the node_path
    -- it is the count of this children nodes that will determine
    -- if the same event is used in another transition
    --
    -- The ltree operators carry an explicit schema. This trigger fires while
    -- pg_restore loads fsm.transition, and pg_restore runs with an empty
    -- search_path, where a bare <@ or @> cannot be resolved and the restore
    -- fails. ltree is installed into public; see set_state_parent_path.sql.
    join fsm.state relative
      on  relative.statechart_id = t.statechart_id
      and (relative.node_path operator(public.<@) s.node_path or relative.node_path operator(public.@>) s.node_path)
      and relative.id <> s.id
    join fsm.transition ct
      on  ct.statechart_id = relative.statechart_id
      and ct.source_state = relative.id
    join changed_event ce
      on  ce.statechart_id = ct.statechart_id
      and ce.event = ct.event
      and ce.statechart_id = t.statechart_id
      and ce.event = t.event;

    if jsonb_array_length(duplicate_transitions) > 0
    then
      raise exception $m$
      Cannot use the same event name for multiple transations in the same tree: %
      $m$, jsonb_pretty(duplicate_transitions);
    end if;

    return null;
  end;
$$ language plpgsql;

set client_min_messages TO warning;
drop trigger if exists check_no_duplicate_event_handler_insert on fsm.transition;
drop trigger if exists check_no_duplicate_event_handler_update on fsm.transition;
reset client_min_messages;

create trigger check_no_duplicate_event_handler_insert
after insert
on fsm.transition
referencing new table as changed
for each statement
execute function fsm.trig_check_no_duplicate_event_handler();

create trigger check_no_duplicate_event_handler_update
after update
on fsm.transition
referencing new table as changed
for each statement
execute function fsm.trig_check_no_duplicate_event_handler();
