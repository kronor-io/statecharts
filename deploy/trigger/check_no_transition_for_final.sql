-- Deploy statecharts:trigger/check_no_transition_for_final to pg

BEGIN;

  create or replace function fsm.trig_check_no_transition_for_final() returns trigger as
  $$
    declare
        bad_states text[];
    begin
      select array_agg(s.id) into bad_states
      from fsm.state s
      join fsm.transition t
        on  t.source_state = s.id
        and t.statechart_id = s.statechart_id
      where s.statechart_id = coalesce(NEW.statechart_id, OLD.statechart_id)
        and s.is_final;

      if (select exists (select unnest(bad_states)))
      then
        raise exception 'cannot add transitions from final states: %', bad_states;
      end if;

      return null;
    end;
  $$ language plpgsql;

  set client_min_messages TO warning;
  drop trigger if exists check_no_transition_for_final on fsm.transition;
  drop trigger if exists check_no_transition_for_final on fsm.state;
  reset client_min_messages;

  create constraint trigger check_no_transition_for_final
  after insert or update
  on fsm.transition
  for each row
  execute function fsm.trig_check_no_transition_for_final();

  create constraint trigger check_no_transition_for_final
  after insert or update
  on fsm.state
  for each row
  when (NEW.is_final is true)
  execute function fsm.trig_check_no_transition_for_final();

COMMIT;
