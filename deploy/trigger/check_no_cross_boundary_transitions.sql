-- Deploy statecharts:trigger/check_no_cross_boundary_transitions to pg

BEGIN;

  create or replace function fsm.trig_check_no_cross_boundary_transition() returns trigger as
  $$
    declare
        bad_transitions jsonb;
    begin
      select jsonb_agg(c.*) into bad_transitions
      -- changed is the table of changes that where just modified
      from changed c
      join fsm.state source
        on  source.statechart_id = c.statechart_id
        and source.id = c.source_state
      join fsm.state target
        on  target.statechart_id = c.statechart_id
        and target.id = c.target_state
      where source.parent_id <> target.parent_id;

      if jsonb_array_length(bad_transitions) > 0
      then
        raise exception 'cannot add transitions between states with different parents: %', jsonb_pretty(bad_transitions);
      end if;

      return null;
    end;
  $$ language plpgsql;

  comment on function fsm.trig_check_no_cross_boundary_transition is $comment$
      This function checks that there are no transitions between states with
      different parents in the "fsm.transition" table.
  $comment$;

  set client_min_messages TO warning;
  drop trigger if exists check_no_cross_boundary_transitions_insert on fsm.transition;
  drop trigger if exists check_no_cross_boundary_transitions_update on fsm.transition;
  reset client_min_messages;

  create trigger check_no_cross_boundary_transitions_insert
  after insert
  on fsm.transition
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_cross_boundary_transition();

  comment on trigger check_no_cross_boundary_transitions_insert on fsm.transition is $comment$
      Checks that no transitions in between states with different parents are
      inserted in the "fsm.transition" table.
  $comment$;

  create trigger check_no_cross_boundary_transitions_update
  after update
  on fsm.transition
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_cross_boundary_transition();

  comment on trigger check_no_cross_boundary_transitions_update on fsm.transition is $comment$
      Checks that no transitions in between states with different parents are
      inserted in the "fsm.transition" table.
  $comment$;

COMMIT;
