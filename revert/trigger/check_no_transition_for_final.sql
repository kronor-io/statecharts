-- Revert statecharts:trigger/check_no_transition_for_final from pg

BEGIN;

  drop trigger if exists check_no_transition_for_final on fsm.transition;
  drop trigger if exists check_no_transition_for_final on fsm.state;
  drop function if exists fsm.trig_check_no_transition_for_final;

COMMIT;
