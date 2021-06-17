-- Revert statecharts:trigger/check_no_cross_boundary_transitions from pg

BEGIN;

  drop trigger if exists check_no_cross_boundary_transitions_insert on fsm.transition;
  drop trigger if exists check_no_cross_boundary_transitions_update on fsm.transition;
  drop function if exists fsm.trig_check_no_cross_boundary_transition;

COMMIT;
