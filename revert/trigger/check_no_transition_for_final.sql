-- Revert statecharts:trigger/check_no_transition_for_final from pg

BEGIN;

  drop trigger if exists check_no_transition_for_final_insert on fsm.transition;
  drop trigger if exists check_no_transition_for_final_update on fsm.transition;
  drop trigger if exists check_no_transition_for_final_update on fsm.state;
  drop function if exists fsm.trig_check_no_transition_for_final;
  drop function if exists fsm.trig_check_no_final_state_with_transition;

COMMIT;
