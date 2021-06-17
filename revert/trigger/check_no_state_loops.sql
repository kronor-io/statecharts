-- Revert statecharts:trigger/check_no_state_loops from pg

BEGIN;

  drop trigger if exists check_no_state_loops_insert on fsm.state;
  drop trigger if exists check_no_state_loops_update on fsm.state;
  drop function if exists fsm.trig_check_no_state_loops;

COMMIT;
