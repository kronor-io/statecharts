-- Revert statecharts:trigger/check_no_state_loops from pg

BEGIN;

  drop trigger if exists check_no_state_loops on fsm.state;
  drop function if exists fsm.trig_check_no_state_loops;

COMMIT;
