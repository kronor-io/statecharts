-- Deploy statecharts:trigger/check_no_transition_for_final to pg

BEGIN;

  create or replace function fsm.trig_check_no_transition_for_final() returns trigger as
  $$
    declare
        bad_transitions jsonb;
    begin
      select jsonb_agg(c.*) into bad_transitions
      from changed c
      join fsm.state s
        on  s.statechart_id = c.statechart_id
        and s.id = c.source_state
        and s.is_final;

      if jsonb_array_length(bad_transitions)
      then
        raise exception 'cannot add transitions from final states: %', jsonb_pretty(bad_transitions);
      end if;

      return null;
    end;
  $$ language plpgsql;

  -- create a similar function for states
  create or replace function fsm.trig_check_no_final_state_with_transition() returns trigger as
  $$
    declare
        bad_states jsonb;
    begin
      select jsonb_agg(c.*) into bad_states
      from changed c
      join fsm.transition t
        on  t.statechart_id = c.statechart_id
        and t.source_state = c.id
      where c.is_final;

      if jsonb_array_length(bad_states)
      then
        raise exception 'cannot add transitions from final states: %', jsonb_pretty(bad_states);
      end if;

      return null;
    end;
  $$ language plpgsql;

  set client_min_messages TO warning;
  drop trigger if exists check_no_transition_for_final_insert on fsm.transition;
  drop trigger if exists check_no_transition_for_final_update on fsm.transition;
  drop trigger if exists check_no_transition_for_final_update on fsm.state;
  reset client_min_messages;

  create trigger check_no_transition_for_final_insert
  after insert
  on fsm.transition
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_transition_for_final();

  create trigger check_no_transition_for_final_update
  after update
  on fsm.transition
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_transition_for_final();

  create trigger check_no_transition_for_final_update
  after update
  on fsm.state
  referencing new table as changed
  for each statement
  execute function fsm.trig_check_no_final_state_with_transition();

COMMIT;
